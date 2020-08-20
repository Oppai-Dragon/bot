module Log
  ( module Log.Console
  , module Log.File
  , module Log.Level
  , module Log
  ) where

import Config.Set

import Log.Console
import Log.File
import Log.Level

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT

data Handle =
  Handle
    { hLogPath :: FilePath
    , hLogLevel :: Maybe Level
    }
  deriving (Show, Eq)

new :: IO Handle
new = do
  config <- setConfig
  logPath <- setLogPath
  let maybeLevel = AT.parseMaybe A.parseJSON (A.Object config)
  return $ Handle logPath maybeLevel
