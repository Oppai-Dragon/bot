module Log.Handle.Builder
  ( new
  ) where

import Config.Set
import Log.File
import Log.Handle

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT

new :: IO Handle
new = do
  config <- setConfig
  logPath <- setLogPath
  let maybeLevel = AT.parseMaybe A.parseJSON (A.Object config)
  return $ Handle logPath maybeLevel
