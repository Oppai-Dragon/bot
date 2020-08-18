module Log.Handle
  ( Handle(..)
  , new
  ) where

import Base
import Config.Set
import Log.File
import Log.Level

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT

data Handle =
  Handle
    { logPath :: FilePath
    , logLevel :: Maybe Level
    }

new :: IO Handle
new = do
  config <- setConfig
  logPath <- setLogPath
  let maybeLevel = AT.parseMaybe A.parseJSON config
  return $ Handle logPath logLevel
