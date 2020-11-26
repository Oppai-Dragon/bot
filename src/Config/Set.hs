module Config.Set
  ( maybeSet
  , maybeSetConfig
  ) where

import Base
import Bot
import Log.Console
import Log.File
import Log.Handle

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Maybe

maybeSet :: FilePath -> IO (Maybe A.Object)
maybeSet path = do
  logPath <- setLogPath
  result <- try $ BSL.readFile path
  case result of
    Right bsl ->
      case A.decode bsl of
        Just hm -> pure $ Just hm
        Nothing -> pure Nothing
    Left err -> do
      logError (Handle logPath Nothing) $ show err
      pure Nothing

maybeSetConfig :: IO (Maybe A.Object)
maybeSetConfig = do
  logPath <- setLogPath
  config <- fromJust <$> maybeSet (setPath "\\configs\\Config.json")
  let maybeBot = AT.parseMaybe (\x -> x A..: "bot" >>= A.parseJSON) сonfig
  let maybeLevel =
        AT.parseMaybe (\x -> x A..: "logLevel" >>= A.parseJSON) config
  let logHandle = Handle logPath maybeLevel
  if isJust maybeBot
    then do
      let botStr = show $ fromJust maybeBot
      logInfo logHandle $ botStr <> " implementation is found"
      let botPath = "\\configs\\" <> botStr <> "\\" <> botStr <> ".json"
      maybeBotConfig <- maybeSet $ setPath botPath
      case maybeBotConfig of
        Just botConfig -> return . Just $ HM.union botConfig config
        Nothing -> return Nothing
    else do
      logError logHandle "Can't find bot implementation"
      return Nothing
