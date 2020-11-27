module Log.Handle.Builder
  ( maybeNew
  ) where

import Config.Set
import Log.File
import Log.Handle

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT

maybeNew :: IO (Maybe Handle)
maybeNew = do
  maybeConfig <- maybeSetConfig
  case maybeConfig of
    Just config -> do
      logPath <- setLogPath
      let maybeLevel =
            AT.parseMaybe (\x -> x A..: "logLevel" >>= A.parseJSON) config
      return $ Just Handle {hLogPath = logPath, hMaybeLogLevel = maybeLevel}
    Nothing -> return Nothing
