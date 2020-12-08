module Config.Create
  ( maybeTCreate
  , maybeTCreateConfig
  ) where

import Base (liftIO)
import Bot
import Log.Console
import Log.File
import Log.Handle

import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM

maybeTCreate :: FilePath -> MaybeT IO A.Object
maybeTCreate path = do
  logPath <- liftIO setLogPath
  result <- liftIO . try $ BSL.readFile path
  case result of
    Right bsl -> MaybeT . return $ A.decode bsl
    Left err -> do
      liftIO . logError Handle {hLogPath = logPath, hMaybeLogLevel = Nothing} $
        show err
      MaybeT $ return Nothing

maybeTCreateConfig :: MaybeT IO A.Object
maybeTCreateConfig = do
  logPath <- liftIO setLogPath
  config <- maybeTCreate "configs/Config.json"
  let maybeBot = AT.parseMaybe (\x -> x A..: "bot" >>= A.parseJSON) config
  let maybeLevel =
        AT.parseMaybe (\x -> x A..: "logLevel" >>= A.parseJSON) config
  let logHandle = Handle {hLogPath = logPath, hMaybeLogLevel = maybeLevel}
  case maybeBot of
    Just bot -> do
      let botStr = show (bot :: Bot)
      liftIO . logInfo logHandle $ botStr <> " implementation is found"
      let botPath = "configs/" <> botStr <> "/" <> botStr <> ".json"
      botConfig <- maybeTCreate botPath
      return $ HM.union botConfig config
    Nothing -> do
      liftIO $ logError logHandle "Can't find bot implementation"
      MaybeT $ return Nothing
