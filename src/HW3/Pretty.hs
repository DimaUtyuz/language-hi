{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module HW3.Pretty
  ( prettyValue,
  )
where

import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import Data.Foldable (Foldable (toList))
import HW3.Base (HiValue (..), HiAction (..))
import Prettyprinter (Doc, Pretty (pretty), viaShow, (<+>), encloseSep, space, parens, dquotes)
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import qualified Data.Map as Map
import Numeric (showHex)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue value = case value of
  -- 0 | -2 | 0.000001 | 1/3 | -4 - 4/7 | 10 + 2/3
  HiValueNumber number -> case fromRationalRepetendUnlimited number of
    (val, Nothing) -> if fracPart == 0
                        then pretty $ numerator number
                        else pretty $ formatScientific Fixed Nothing val
    _ -> signNumber <> case (integerPart, fracPart) of
                        (_, 0) -> pretty integerPart
                        (0, _) -> frac
                        _ -> pretty integerPart <+> signJoin <+> frac
    where
      absNumber = abs number
      den = denominator absNumber
      integerPart = quot (numerator absNumber) den
      fracPart = absNumber - toRational integerPart
      frac = pretty (numerator fracPart) <> "/" <> pretty (denominator fracPart)
      signJoin = if number < 0 then "-" else "+"
      signNumber = if number < 0 then "-" else ""
  -- add
  HiValueFunction func -> viaShow func
  -- true
  HiValueBool b -> if b then "true" else "false"
  -- null
  HiValueNull -> "null"
  -- "hello"
  HiValueString str -> viaShow str
  -- [ true, null, 3, "hello" ]
  HiValueList list -> showElements "[" "]" ", " prettyValue $ toList list
  -- [# 00 ef #]
  HiValueBytes bytes -> showElements "[#" "#]" " " showByte $ BS.unpack bytes
    where
      showByte :: Word8 -> Doc AnsiStyle
      showByte b = (if b < 16 then "0" else "") <> pretty (showHex (toInteger b) "")
  -- write("hello/world", [# 79 65 73 #])
  HiValueAction action -> case action of
    HiActionRead filePath -> showInBrackets "read" $ viaShow filePath
    HiActionWrite filePath bytes -> showInBrackets "write" $ viaShow filePath <> "," <+> prettyValue (HiValueBytes bytes)
    HiActionMkDir filePath -> showInBrackets "mkdir" $ viaShow filePath
    HiActionChDir filePath -> showInBrackets "cd" $ viaShow filePath
    HiActionCwd -> "cwd"
    HiActionNow -> "now"
    HiActionRand l r -> showInBrackets "rand" $ pretty l <> ", " <> pretty r
    HiActionEcho str -> showInBrackets "echo" $ viaShow str
  -- parse-time("2021-12-15 00:16:40 UTC")
  HiValueTime time -> showInBrackets "parse-time" $ dquotes $ viaShow time
  -- { true: null, 3: "hello" }
  HiValueDict dict -> showElements "{" "}" ", " showEntry $ Map.toList dict
    where
      showEntry :: (HiValue, HiValue) -> Doc AnsiStyle
      showEntry (key, val) = prettyValue key <> ": " <> prettyValue val
  where
    showElements :: String -> String -> String -> (a -> Doc AnsiStyle) -> [a] -> Doc AnsiStyle
    showElements l r s mapper list = if null list
                                      then pretty l <+> pretty r
                                      else encloseSep (pretty l <> space) (space <> pretty r) (pretty s) $ map mapper list

    showInBrackets :: String -> Doc AnsiStyle -> Doc AnsiStyle
    showInBrackets name body = pretty name <> parens body
