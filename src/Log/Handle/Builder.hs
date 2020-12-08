module Log.Handle.Builder
  ( maybeNew
  ) where

import Base (liftIO)
import Config.Create (maybeTCreateConfig)
import Log.File (setLogPath)
import Log.Handle (Handle(..))

import Control.Monad.Trans.Maybe (MaybeT)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT

maybeNew :: MaybeT IO Handle
maybeNew = do
  config <- maybeTCreateConfig
  logPath <- liftIO setLogPath
  let maybeLevel =
        AT.parseMaybe (\x -> x A..: "logLevel" >>= A.parseJSON) config
  return Handle {hLogPath = logPath, hMaybeLogLevel = maybeLevel}
