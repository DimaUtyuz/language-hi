module Main
  ( main,
    hi,
    hiAcl,
  )
  where

import Control.Exception (catches, Handler (..), IOException, SomeException)
import Data.Set (Set, singleton, empty)
import Data.Void (Void)
import HW3.Action (HIO (runHIO), HiPermission (..), PermissionException)
import HW3.Base (HiError, HiValue)
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Console.Haskeline (defaultSettings, getInputLine, runInputT, InputT, outputStrLn)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import Control.Monad.Trans.Class (lift)

main :: IO ()
main = putStrLn "main"

-- start hi repl with all permissions
hi :: IO ()
hi = hiAcl "rwt"

-- start hi repl with given permissions: "wt" - write and time
-- w(AllowWrite), r(AllowRead), t(AllowRead)
hiAcl :: String -> IO ()
hiAcl aclStr = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just str -> (lift (parseAndEval str) >>= outputStrLn) <* loop

    parseAndEval :: String -> IO String
    parseAndEval str =
      catches
        (fmap toString (mapM (flip runHIO aclSet . eval) . parse $ str))
        [Handler catchPermissionException, Handler catchIOException, Handler catchAnyException]

    catchIOException :: IOException -> IO String
    catchIOException e = pure $ "IO exception occurred while executing the action(!): " <> show e

    catchPermissionException :: PermissionException -> IO String
    catchPermissionException e = pure $ "Not enough permissions: " <> show e

    catchAnyException :: SomeException -> IO String
    catchAnyException e = pure $ "Unexpected exception: " <> show e

    toString :: Either (ParseErrorBundle String Void) (Either HiError HiValue) -> String
    toString parseResult = case parseResult of
                              Left parseError -> errorBundlePretty parseError
                              Right evalResult -> case evalResult of
                                Left evalError  -> show evalError
                                Right value -> renderString $ layoutPretty defaultLayoutOptions $ prettyValue value

    aclSet :: Set HiPermission
    aclSet = getPermissionIfAllow 'r' AllowRead <> getPermissionIfAllow 'w' AllowWrite <> getPermissionIfAllow 't' AllowTime
      where
        getPermissionIfAllow :: Char -> HiPermission -> Set HiPermission
        getPermissionIfAllow mode permission = if mode `elem` aclStr then singleton permission else empty
