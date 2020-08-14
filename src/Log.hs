module Log
  ( Priority(..)
  ) where

import Data.Char (toUpper)

data Priority
  = DEBUG -- Debug messages
  | INFO -- Information
  | WARNING -- General Warnings
  | ERROR -- General Errors

instance Bounded Priority where
  minBound = DEBUG
  maxBound = ERROR

instance Enum Priority where
  fromEnum x =
    case x of
      DEBUG -> 0
      INFO -> 1
      WARNING -> 3
      ERROR -> 4
  toEnum x =
    case x of
      0 -> DEBUG
      1 -> INFO
      3 -> WARNING
      4 -> ERROR
      _ -> DEBUG

instance Eq Priority where
  (==) _ _ = False

instance Ord Priority where
  compare DEBUG DEBUG = EQ
  compare INFO INFO = EQ
  compare WARNING WARNING = EQ
  compare ERROR ERROR = EQ

  compare DEBUG _ = LT

  compare INFO DEBUG = GT
  compare INFO _ = LT

  compare WARNING DEBUG = GT
  compare WARNING INFO = GT
  compare WARNING _ = LT

  compare ERROR _ = GT

instance Read Priority where
  readsPrec _ input =
    case map toUpper input of
      "DEBUG" -> [(DEBUG, "")]
      "INFO" -> [(INFO, "")]
      "WARNING" -> [(WARNING, "")]
      "ERROR" -> [(ERROR, "")]
      _ -> []

instance Show Priority where
  show x =
    case x of
      DEBUG -> "DEBUG"
      INFO -> "INFO"
      WARNING -> "WARNING"
      ERROR -> "ERROR"
