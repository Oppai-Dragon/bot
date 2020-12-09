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
import qualified System.IO as IO

maybeNew :: MaybeT IO Handle
maybeNew = do
  config <- maybeTCreateConfig
  fileHandle <- liftIO $ setLogPath >>= \path ->
    IO.writeFile path "\n" >> IO.openFile path IO.AppendMode
  let maybeLevel =
        AT.parseMaybe (\x -> x A..: "logLevel" >>= A.parseJSON) config
  return Handle {hLogMaybeLevel = maybeLevel, hLogFileHandle = fileHandle}
