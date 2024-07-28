{-# LANGUAGE DerivingVia #-}

module HW3.Action
  ( HiPermission (..),
    PermissionException (..),
    HIO (..),
  )
where

import Control.Exception (Exception, throwIO)
import Data.Set (Set, member)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad (unless)
import HW3.Base (HiMonad(..), HiAction(..), HiValue(..))
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         listDirectory, setCurrentDirectory)
import qualified Data.ByteString as ByteString
import Data.Text.Encoding (decodeUtf8')
import Data.Text (pack)
import qualified Data.Text.IO as IO
import Data.Sequence (fromList)
import Data.Time (getCurrentTime)
import System.Random (getStdRandom, uniformR)
import Control.Monad.Trans.Reader (ReaderT (..))

-- system permissions
data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Eq, Ord, Enum, Bounded)

instance Show HiPermission where
  show AllowRead = "read"
  show AllowWrite = "write"
  show AllowTime = "time"

newtype PermissionException = PermissionRequired HiPermission deriving Eq

instance Show PermissionException where
  show (PermissionRequired perm) = "No " <> show perm <> " permission"

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving(Functor, Applicative, Monad, MonadIO)
    via ReaderT (Set HiPermission) IO

instance HiMonad HIO where
  runAction a = case a of
    -- writes bytes to given path
    HiActionWrite filePath bytes -> HiValueNull <$ checkPermissionAndApply AllowWrite (ByteString.writeFile filePath) bytes
    -- makes directory by given path
    HiActionMkDir filePath -> HiValueNull <$ checkPermissionAndApply AllowWrite createDirectory filePath
    -- changes current directory to given path
    HiActionChDir filePath -> HiValueNull <$ checkPermissionAndApply AllowRead setCurrentDirectory filePath
    -- returns current directory
    HiActionCwd -> HiValueString . pack <$> checkPermissionAndApply AllowRead (const getCurrentDirectory) Nothing
    -- returns current time
    HiActionNow -> HiValueTime <$> checkPermissionAndApply AllowTime (const getCurrentTime) Nothing
    -- returns random integer number from given range inclusive
    HiActionRand left right -> HiValueNumber . fromIntegral <$> getStdRandom (uniformR (left, right))
    -- prints given string
    HiActionEcho str -> HiValueNull <$ checkPermissionAndApply AllowWrite IO.putStrLn str
    -- tries to read string otherwise bytes from given path
    HiActionRead filePath -> checkPermissionAndApply AllowRead getFileOrListDirectory filePath
      where
        getFileOrListDirectory :: FilePath -> IO HiValue
        getFileOrListDirectory fp = do
          isFile <- doesFileExist fp
          if isFile
            then (\b -> either (const $ HiValueBytes b) HiValueString $ decodeUtf8' b) <$> ByteString.readFile filePath
            else getListDirectory fp

        getListDirectory :: FilePath -> IO HiValue
        getListDirectory fp = do
          isDirectory <- doesDirectoryExist fp
          if isDirectory
            then HiValueList . fromList . map (HiValueString . pack) <$> listDirectory filePath
            else pure HiValueNull
    where
      checkPermissionAndApply :: HiPermission -> (a -> IO b) -> a -> HIO b
      checkPermissionAndApply needPermission func arg = HIO $ \allowPermissions -> do
        unless (member needPermission allowPermissions) $ throwIO (PermissionRequired needPermission)
        func arg
