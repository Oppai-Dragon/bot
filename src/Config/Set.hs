module Config.Set
  ( set
  , setConfig
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

set :: FilePath -> IO A.Object
set path = do
  logPath <- setLogPath
  result <- tryM $ BSL.readFile path
  case result of
    Right bsl ->
      case A.decode bsl of
        Just hm -> pure hm
        Nothing -> pure HM.empty
    Left err -> do
      errorM (Handle logPath Nothing) $ show err
      pure HM.empty

setConfig :: IO A.Object
setConfig = do
  logPath <- setLogPath
  config <- set $ setPath "\\src\\Config.json"
  let maybeBot = AT.parseMaybe (\x -> x A..: "bot" >>= A.parseJSON) config
  let maybeLevel =
        AT.parseMaybe (\x -> x A..: "logLevel" >>= A.parseJSON) config
  let logHandle = Handle logPath maybeLevel
  botStr <-
    fmap show $
    case maybeBot of
      Just x -> do
        infoM logHandle $ show x <> " implementation is found"
        return x
      Nothing -> do
        errorM
          logHandle
          "Can't find bot implementation, check his name in Config.json"
        infoM logHandle "Will be used Vk implementation"
        return Vk
  let botPath = "\\src\\Bot\\" <> botStr <> "\\" <> botStr <> ".json"
  botConfig <- set $ setPath botPath
  return $ HM.union botConfig config
