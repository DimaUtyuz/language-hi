{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module HW3.Evaluator
  ( eval,
  )
where

import Control.Monad (foldM, unless, when)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Semigroup (stimes)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import HW3.Base (HiError (..), HiExpr (..), HiFun (..), HiValue (..), HiMonad (..), HiAction (..))
import Data.Ratio (denominator)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy
import Data.Foldable (toList)
import Data.Word (Word8)
import Codec.Serialise (deserialise, serialise)
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Text.Read as TextRead
import Data.Time (addUTCTime, diffUTCTime)
import Data.Map (Map, assocs, insertWith)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Codec.Compression.Zlib (CompressParams (compressLevel), bestCompression, compressWith,
                               decompressWith, defaultCompressParams, defaultDecompressParams)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT (evalExpr expr)

-- evaluates expression to value
evalExpr :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalExpr expr = case expr of
  HiExprValue val -> pure val

  HiExprApply funcExpr argsExpr -> do
    func <- evalExpr funcExpr
    case func of
      HiValueFunction f -> evalFunction f argsExpr
      HiValueString str -> evalIterable str argsExpr
      HiValueList list -> evalIterable list argsExpr
      HiValueBytes bytes -> evalIterable bytes argsExpr

      HiValueDict dict -> case argsExpr of
        [fieldExpr] -> getField <$> evalExpr fieldExpr
        _ -> throwE HiErrorArityMismatch
        where
          getField :: HiValue -> HiValue
          getField field = fromMaybe HiValueNull $ Map.lookup field dict

      _ -> throwE HiErrorInvalidFunction

  HiExprRun runnableExpr -> do
    runnable <- evalExpr runnableExpr
    case runnable of
      HiValueAction action -> lift $ runAction action
      _ -> throwE HiErrorInvalidArgument

  HiExprDict entriesExpr -> do
    entries <- mapM evalEntry entriesExpr
    pure (HiValueDict $ Map.fromList entries)
    where
      evalEntry :: HiMonad m => (HiExpr, HiExpr) -> ExceptT HiError m (HiValue, HiValue)
      evalEntry (a, b) = (,) <$> evalExpr a <*> evalExpr b

-- evaluates function with given list of expressions
evalFunction :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalFunction func args = do
  -- check number of args for function
  unless (checkArgsSize func (length args)) $ throwE HiErrorArityMismatch
  case func of
    -- evaluates OR operation lazy: doesn't evaluate right expression if left isn't Null or False
    HiFunOr -> case args of
          [lExpr, rExpr] -> do
            left <- evalExpr lExpr
            case left of
              HiValueNull -> evalExpr rExpr
              HiValueBool False -> evalExpr rExpr
              _ -> pure left
          _ -> throwE HiErrorInvalidArgument
    -- evaluates AND operation lazy: doesn't evaluate right expression if left is Null or False
    HiFunAnd -> case args of
          [lExpr, rExpr] -> do
            left <- evalExpr lExpr
            case left of
              HiValueNull -> pure HiValueNull
              HiValueBool False -> pure (HiValueBool False)
              _ -> evalExpr rExpr
          _ -> throwE HiErrorInvalidArgument
    -- evaluates IF operation lazy: evaluates only need branch
    HiFunIf -> case args of
      [conditionExpr, thenExpr, elseExpr] -> do
        condition <- evalExpr conditionExpr
        case condition of
          HiValueBool b -> if b then evalExpr thenExpr else evalExpr elseExpr
          _ -> throwE HiErrorInvalidArgument
      _ -> throwE HiErrorInvalidArgument
    -- evaluates other function with evaluating all arguments
    _ -> do
      params <- mapM evalExpr args
      evalFunctionWithValues func params

-- evaluates function with given list of values
evalFunctionWithValues :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
evalFunctionWithValues func params = case func of
  HiFunAdd -> case params of
    [HiValueNumber l, HiValueNumber r] -> pure (HiValueNumber $ l + r)
    [HiValueString l, HiValueString r] -> pure (HiValueString $ l <> r)
    [HiValueList l, HiValueList r] -> pure (HiValueList $ l <> r)
    [HiValueBytes l, HiValueBytes r] -> pure (HiValueBytes $ l <> r)
    [HiValueTime time, HiValueNumber delta] -> pure (HiValueTime $ addUTCTime (realToFrac delta) time)
    _ -> throwE HiErrorInvalidArgument
  HiFunSub -> case params of
    [HiValueNumber l, HiValueNumber r] -> pure (HiValueNumber $ l - r)
    [HiValueTime l, HiValueTime r] -> pure (HiValueNumber $ toRational $ diffUTCTime l r)
    _ -> throwE HiErrorInvalidArgument
  HiFunMul -> case params of
    [HiValueNumber l, HiValueNumber r] -> pure (HiValueNumber $ l * r)
    [HiValueString str, HiValueNumber n] -> repeatValue n HiValueString str
    [HiValueList list, HiValueNumber n] -> repeatValue n HiValueList list
    [HiValueBytes bytes, HiValueNumber n] -> repeatValue n HiValueBytes bytes
    _ -> throwE HiErrorInvalidArgument
  HiFunDiv -> case params of
    [HiValueNumber l, HiValueNumber r] -> do
      when (r == 0) $ throwE HiErrorDivideByZero
      pure (HiValueNumber $ l / r)
    [HiValueString l, HiValueString r] -> pure (HiValueString $ l <> "/" <> r)
    _ -> throwE HiErrorInvalidArgument

  HiFunNot -> case params of
    [HiValueBool b] -> pure (HiValueBool $ not b)
    _ -> throwE HiErrorInvalidArgument
  HiFunAnd -> case params of
    [HiValueNull, _] -> pure HiValueNull
    [HiValueBool False, _] -> pure (HiValueBool False)
    [_, r] -> pure r
    _ -> throwE HiErrorInvalidArgument
  HiFunOr -> case params of
    [HiValueNull, r] -> pure r
    [HiValueBool False, r] -> pure r
    [l, _] -> pure l
    _ -> throwE HiErrorInvalidArgument

  HiFunLessThan -> compareValues (<) params
  HiFunGreaterThan -> compareValues (>) params
  HiFunEquals -> compareValues (==) params
  HiFunNotLessThan -> compareValues (>=) params
  HiFunNotGreaterThan -> compareValues (<=) params
  HiFunNotEquals -> compareValues (/=) params

  HiFunIf -> case params of
    [HiValueBool True, l, _] -> pure l
    [HiValueBool False, _, r] -> pure r
    _ -> throwE HiErrorInvalidArgument

  HiFunToLower -> mapString Text.toLower params
  HiFunToUpper -> mapString Text.toUpper params
  HiFunTrim -> mapString Text.strip params

  HiFunLength -> case params of
    [HiValueString str] -> pure (getLength str)
    [HiValueList list] -> pure (getLength list)
    [HiValueBytes bytes] -> pure (getLength bytes)
    _ -> throwE HiErrorInvalidArgument
  HiFunReverse -> case params of
    [HiValueString s] -> pure (HiValueString $ Text.reverse s)
    [HiValueList l] -> pure (HiValueList $ Seq.reverse l)
    [HiValueBytes bytes] -> pure (HiValueBytes $ ByteString.reverse bytes)
    _ -> throwE HiErrorInvalidArgument

  HiFunList -> pure (HiValueList $ Seq.fromList params)
  HiFunRange -> case params of
    [HiValueNumber l, HiValueNumber r] -> pure (HiValueList $ HiValueNumber <$> [l .. r])
    _ -> throwE HiErrorInvalidArgument
  HiFunFold -> case params of
    [HiValueFunction f, HiValueList list] -> do
      unless (checkArgsSize f 2) $ throwE HiErrorArityMismatch
      case getValues list of
        [] -> pure HiValueNull
        (h : t) -> foldM (\b a -> evalFunctionWithValues f [b, a]) h t
    _ -> throwE HiErrorInvalidArgument

  HiFunPackBytes -> case params of
    [HiValueList values] -> do
      unless (all checkConvert values) $ throwE HiErrorInvalidArgument
      pure (HiValueBytes . ByteString.pack . toList $ toWord8 <$> values)
        where
          checkConvert :: HiValue -> Bool
          checkConvert v = case v of
            (HiValueNumber n) -> toRational (minBound::Word8) <= n && n <= toRational (maxBound::Word8) && (denominator n == 1)
            _ -> False

          toWord8 :: HiValue -> Word8
          toWord8 v = case v of
            (HiValueNumber n) -> fromInteger $ truncate n
            _ -> 0
    _ -> throwE HiErrorInvalidArgument
  HiFunUnpackBytes -> case params of
    [HiValueBytes bytes] -> pure (HiValueList . Seq.fromList $ getValues bytes)
    _ -> throwE HiErrorInvalidArgument

  HiFunEncodeUtf8 -> case params of
    [HiValueString str] -> pure (HiValueBytes $ TextEncoding.encodeUtf8 str)
    _ -> throwE HiErrorInvalidArgument
  HiFunDecodeUtf8 -> case params of
    [HiValueBytes bytes] -> pure (decode bytes)
      where
        decode :: ByteString.ByteString -> HiValue
        decode values = case TextEncoding.decodeUtf8' values of
          (Left _) -> HiValueNull
          (Right s) -> HiValueString s
    _ -> throwE HiErrorInvalidArgument

  HiFunZip -> wrapZip (compressWith defaultCompressParams { compressLevel = bestCompression }) params
  HiFunUnzip -> wrapZip (decompressWith defaultDecompressParams) params

  HiFunSerialise -> case params of
    [object] -> pure (HiValueBytes $ ByteStringLazy.toStrict $ serialise object)
    _ -> throwE HiErrorInvalidArgument
  HiFunDeserialise -> case params of
    [HiValueBytes bytes] -> pure (deserialise $ ByteStringLazy.fromStrict bytes)
    _ -> throwE HiErrorInvalidArgument

  HiFunRead -> createFilePathAction HiActionRead params
  HiFunWrite -> case params of
    [HiValueString filePath, HiValueString str] -> pure (writeAction filePath $ TextEncoding.encodeUtf8 str)
    [HiValueString filePath, HiValueBytes bytes] -> pure (writeAction filePath bytes)
    _ -> throwE HiErrorInvalidArgument
    where
      writeAction :: Text.Text -> ByteString.ByteString -> HiValue
      writeAction filePath bytes = HiValueAction $ HiActionWrite (Text.unpack filePath) bytes
  HiFunMkDir -> createFilePathAction HiActionMkDir params
  HiFunChDir -> createFilePathAction HiActionChDir params

  HiFunParseTime -> case params of
    [HiValueString timeStr] -> case TextRead.readMaybe (Text.unpack timeStr) of
      Nothing -> pure HiValueNull
      Just time -> pure (HiValueTime time)
    _ -> throwE HiErrorInvalidArgument

  HiFunRand -> case params of
    [HiValueNumber l, HiValueNumber r] -> do
      from <- evalInt l
      to <- evalInt r
      pure (HiValueAction $ HiActionRand from to)
    _ -> throwE HiErrorInvalidArgument

  HiFunEcho -> case params of
    [HiValueString str] -> pure (HiValueAction $ HiActionEcho str)
    _ -> throwE HiErrorInvalidArgument

  HiFunKeys -> case params of
    [HiValueDict dict] -> pure (HiValueList $ Seq.fromList $ Map.keys dict)
    _ -> throwE HiErrorInvalidArgument
  HiFunValues -> case params of
    [HiValueDict dict] -> pure (HiValueList $ Seq.fromList $ Map.elems dict)
    _ -> throwE HiErrorInvalidArgument
  HiFunInvert -> case params of
    [HiValueDict dict] -> pure (HiValueDict $ Map.map (HiValueList . Seq.fromList) $ invertDict dict)
    _ -> throwE HiErrorInvalidArgument
    where
      invertDict :: Map HiValue HiValue -> Map HiValue [HiValue]
      invertDict dict = foldl insertToValueList Map.empty $ assocs dict

      insertToValueList :: Map HiValue [HiValue] -> (HiValue, HiValue) -> Map HiValue [HiValue]
      insertToValueList dict (key, value) = insertWith (++) value [key] dict
  HiFunCount -> case params of
    [HiValueString str] -> pure (countValues str)
    [HiValueList list] -> pure (countValues list)
    [HiValueBytes bytes] -> pure (countValues bytes)
    _ -> throwE HiErrorInvalidArgument
    where
      countValues :: (HiIterable a) => a -> HiValue
      countValues iterable = wrapMap $ foldl countValue Map.empty $ getValues iterable

      countValue :: Map HiValue Int -> HiValue -> Map HiValue Int
      countValue dict value = insertWith (+) value 1 dict

      wrapMap :: Map HiValue Int -> HiValue
      wrapMap dict = HiValueDict $ Map.map (HiValueNumber . toRational) dict
  where
    compareValues :: (HiMonad m) => (HiValue -> HiValue -> Bool) -> [HiValue] -> ExceptT HiError m HiValue
    compareValues comp args = case args of
      [l, r] -> pure (HiValueBool $ comp l r)
      _ -> throwE HiErrorInvalidArgument

    mapString :: (HiMonad m) => (Text.Text -> Text.Text) -> [HiValue] -> ExceptT HiError m HiValue
    mapString mapper args = case args of
      [HiValueString str] -> pure (HiValueString $ mapper str)
      _ -> throwE HiErrorInvalidArgument

    wrapZip :: (HiMonad m) => (ByteStringLazy.ByteString -> ByteStringLazy.ByteString) -> [HiValue] -> ExceptT HiError m HiValue
    wrapZip zipper args = case args of
      [HiValueBytes bytes] -> pure (HiValueBytes $ ByteStringLazy.toStrict $ zipper $ ByteStringLazy.fromStrict bytes)
      _ -> throwE HiErrorInvalidArgument

    createFilePathAction :: (HiMonad m) => (FilePath -> HiAction) -> [HiValue] -> ExceptT HiError m HiValue
    createFilePathAction action args = case args of
      [HiValueString filePath] -> pure (HiValueAction $ action $ Text.unpack filePath)
      _ -> throwE HiErrorInvalidArgument

-- class for evaluating functions such as slice or index
class HiIterable a where
  index :: a -> Int -> HiValue
  slice :: a -> Int -> Int -> HiValue
  len :: a -> Int
  getValues :: a -> [HiValue]

  getLength :: a -> HiValue
  getLength iterable = HiValueNumber . toRational $ len iterable

  getOrNull :: a -> Int -> HiValue
  getOrNull iterable ind = if 0 <= ind && ind < len iterable
                            then index iterable ind
                            else HiValueNull

  getSliceOrEmpty :: a -> Int -> Int -> HiValue
  getSliceOrEmpty iterable from to = slice iterable (max 0 from) (min to $ len iterable)

instance HiIterable (Seq.Seq HiValue) where
  index arr times = Seq.index arr times
  slice arr from to = HiValueList $ Seq.take (to - from) $ Seq.drop from arr
  len = Seq.length
  getValues = toList

instance HiIterable Text.Text where
  index str ind = HiValueString $ Text.pack [Text.index str ind]
  slice str from to = HiValueString $ Text.take (to - from) $ Text.drop from str
  len = Text.length
  getValues str = HiValueString . Text.singleton <$> Text.unpack str

instance HiIterable ByteString.ByteString where
  index bytes ind = HiValueNumber . toRational $ ByteString.index bytes ind
  slice bytes from to = HiValueBytes $ ByteString.take (to - from) $ ByteString.drop from bytes
  len = ByteString.length
  getValues bytes = HiValueNumber . toRational <$> ByteString.unpack bytes

-- makes an integer from the rational number with a check of the fractional part for equality to zero and falling into the range of Int
evalInt :: (HiMonad m) => Rational -> ExceptT HiError m Int
evalInt ind = if (denominator ind == 1) && fromIntegral (minBound :: Int) <= ind && ind <= fromIntegral (maxBound :: Int)
              then pure (truncate ind)
              else throwE HiErrorInvalidArgument

-- returns value from given collection by given index
getByIndex :: (HiIterable a, HiMonad m) => a -> HiValue -> ExceptT HiError m HiValue
getByIndex iterable indValue = case indValue of
  (HiValueNumber ind') -> do
    ind <- evalInt ind'
    pure (getOrNull iterable ind)
  _ -> throwE HiErrorInvalidArgument

-- returns collection of elements from given collection by given range, supports negative indexing from python
getSlice :: (HiIterable a, HiMonad m) => a -> HiValue -> HiValue -> ExceptT HiError m HiValue
getSlice iterable fromValue toValue = case (fromValue, toValue) of
  (HiValueNumber from', HiValueNumber to') -> do
    from <- evalInt from'
    to <- evalInt to'
    pure (getSliceOrEmpty iterable (fixNegativeIndex from) (fixNegativeIndex to))
    where
      fixNegativeIndex :: Int -> Int
      fixNegativeIndex ind = if ind < 0 then ind + len iterable else ind
  (HiValueNull, _) -> getSlice iterable (HiValueNumber 0) toValue
  (_, HiValueNull) -> getSlice iterable fromValue (HiValueNumber . toRational $ len iterable)
  _ -> throwE HiErrorInvalidArgument

-- evaluates getting by index or slicing for given collection
evalIterable :: (HiIterable a, HiMonad m) => a -> [HiExpr] -> ExceptT HiError m HiValue
evalIterable iterable args = case args of
  [indExpr] -> do
    indValue <- evalExpr indExpr
    getByIndex iterable indValue
  [fromExpr, toExpr] -> do
    fromValue <- evalExpr fromExpr
    toValue <- evalExpr toExpr
    getSlice iterable fromValue toValue
  _ -> throwE HiErrorArityMismatch

-- repeats value given number times
repeatValue :: (HiMonad m, Semigroup a) => Rational -> (a -> HiValue) -> a -> ExceptT HiError m HiValue
repeatValue n wrapper values = do
  times <- evalInt n
  when (times <= 0) $ throwE HiErrorInvalidArgument
  pure (wrapper $ stimes times values)

-- checks if given number matches the number of expected arguments for given function
checkArgsSize :: HiFun -> Int -> Bool
checkArgsSize func = case func of
  HiFunAdd -> (2 ==)
  HiFunSub -> (2 ==)
  HiFunMul -> (2 ==)
  HiFunDiv -> (2 ==)
  HiFunNot -> (1 ==)
  HiFunAnd -> (2 ==)
  HiFunOr -> (2 ==)
  HiFunLessThan -> (2 ==)
  HiFunGreaterThan -> (2 ==)
  HiFunEquals -> (2 ==)
  HiFunNotLessThan -> (2 ==)
  HiFunNotGreaterThan -> (2 ==)
  HiFunNotEquals -> (2 ==)
  HiFunIf -> (3 ==)
  HiFunLength -> (1 ==)
  HiFunToUpper -> (1 ==)
  HiFunToLower -> (1 ==)
  HiFunReverse -> (1 ==)
  HiFunTrim -> (1 ==)
  HiFunList -> const True
  HiFunRange -> (2 ==)
  HiFunFold -> (2 ==)
  HiFunPackBytes -> (1 ==)
  HiFunUnpackBytes -> (1 ==)
  HiFunEncodeUtf8 -> (1 ==)
  HiFunDecodeUtf8 -> (1 ==)
  HiFunZip -> (1 ==)
  HiFunUnzip -> (1 ==)
  HiFunSerialise -> (1 ==)
  HiFunDeserialise -> (1 ==)
  HiFunRead -> (1 ==)
  HiFunWrite -> (2 ==)
  HiFunMkDir -> (1 ==)
  HiFunChDir -> (1 ==)
  HiFunParseTime -> (1 ==)
  HiFunRand -> (2 ==)
  HiFunEcho -> (1 ==)
  HiFunCount -> (1 ==)
  HiFunKeys -> (1 ==)
  HiFunValues -> (1 ==)
  HiFunInvert -> (1 ==)
