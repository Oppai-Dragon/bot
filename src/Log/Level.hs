module Log.Level
  ( Level(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T

data Level
  = DEBUG -- Debug messages
  | INFO -- Information
  | WARNING -- General Warnings
  | ERROR -- General Errors
  deriving (Show, Read, Eq, Ord, Bounded)

instance A.FromJSON Level where
  parseJSON =
    A.withText "From JSON Log.Level.Level" $ \x ->
      case x of
        "debug" -> pure DEBUG
        "info" -> pure INFO
        "warning" -> pure WARNING
        "error" -> pure ERROR
        _ -> fail $ "Unknown log level: " <> T.unpack x
