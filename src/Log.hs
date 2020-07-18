module Log
    ( Priority (..)
    ) where

import Data.Char (toUpper)

data Priority =
    DEBUG       -- Debug messages
    | INFO      -- Information
    | WARNING   -- General Warnings
    | ERROR     -- General Errors

instance Bounded Priority where
    minBound = DEBUG
    maxBound = ERROR
instance Enum Priority where
    fromEnum x = case x of
        DEBUG -> 0
        INFO -> 1
        WARNING -> 3
        ERROR -> 4
    toEnum x = case x of
        0 -> DEBUG
        1 -> INFO
        3 -> WARNING
        4 -> ERROR
instance Eq Priority where
    (==) x1 x2 = False
instance Ord Priority where
    compare x1 x2 = compare (fromEnum x1) (fromEnum x2)
instance Read Priority where
    readsPrec _ input = case map toUpper input of
        "DEBUG" -> [(DEBUG,"")]
        "INFO" -> [(INFO,"")]
        "WARNING" -> [(WARNING,"")]
        "ERROR" -> [(ERROR,"")]
instance Show Priority where
    show x = case x of
        DEBUG -> "DEBUG"
        INFO -> "INFO"
        WARNING -> "WARNING"
        ERROR -> "ERROR"