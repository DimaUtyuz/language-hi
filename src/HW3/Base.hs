{-# LANGUAGE DeriveGeneric #-}

module HW3.Base
  ( HiError (..),
    HiExpr (..),
    HiFun (..),
    HiValue (..),
    HiMonad (..),
    HiAction (..),
  )
where

import Data.Sequence (Seq)
import Data.ByteString (ByteString)
import Codec.Serialise (Serialise)
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Data.Map (Map)

data HiFun -- function names
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Ord, Generic)

instance Show HiFun where
  show HiFunDiv = "div"
  show HiFunMul = "mul"
  show HiFunAdd = "add"
  show HiFunSub = "sub"
  show HiFunNot = "not"
  show HiFunAnd = "and"
  show HiFunOr = "or"
  show HiFunLessThan = "less-than"
  show HiFunGreaterThan = "greater-than"
  show HiFunEquals = "equals"
  show HiFunNotLessThan = "not-less-than"
  show HiFunNotGreaterThan = "not-greater-than"
  show HiFunNotEquals = "not-equals"
  show HiFunIf = "if"
  show HiFunLength = "length"
  show HiFunToUpper = "to-upper"
  show HiFunToLower = "to-lower"
  show HiFunReverse = "reverse"
  show HiFunTrim = "trim"
  show HiFunList = "list"
  show HiFunRange = "range"
  show HiFunFold = "fold"
  show HiFunPackBytes = "pack-bytes"
  show HiFunUnpackBytes = "unpack-bytes"
  show HiFunEncodeUtf8 = "encode-utf8"
  show HiFunDecodeUtf8 = "decode-utf8"
  show HiFunZip = "zip"
  show HiFunUnzip = "unzip"
  show HiFunSerialise = "serialise"
  show HiFunDeserialise = "deserialise"
  show HiFunRead = "read"
  show HiFunWrite = "write"
  show HiFunMkDir = "mkdir"
  show HiFunChDir = "cd"
  show HiFunParseTime = "parse-time"
  show HiFunRand = "rand"
  show HiFunEcho = "echo"
  show HiFunCount = "count"
  show HiFunKeys = "keys"
  show HiFunValues = "values"
  show HiFunInvert = "invert"

instance Serialise HiFun

data HiValue -- values
  = HiValueNull
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Eq, Ord, Show, Generic)

instance Serialise HiValue

data HiExpr -- expressions
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Eq, Ord, Show)

data HiError -- evaluation errors
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Eq, Ord, Show)

data HiAction = -- actions
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Eq, Ord, Show, Generic)

instance Serialise HiAction

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
