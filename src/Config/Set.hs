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
    Left err -> errorM (Handle logPath Nothing) (show err) >> pure HM.empty

setConfig :: IO A.Object
setConfig = do
  repDir <- getRepDir
  logPath <- setLogPath
  config <- set $ repDir <> "\\src\\Config.json"
  let maybeBot = AT.parseMaybe (\x -> x A..: "bot" >>= A.parseJSON) config
  let maybeLevel =
        AT.parseMaybe (\x -> x A..: "logLevel" >>= A.parseJSON) config
  let logHandle = Handle logPath maybeLevel
  let errorLog =
        errorM
          logHandle
          "Can't find bot implementation, check his name in Config.json"
  let infoLog = infoM logHandle "Bot implementation is found"
  bot <-
    case maybeBot of
      Just x -> infoLog >> return x
      Nothing ->
        errorLog >> infoM logHandle "Will be used Vk implementation" >>
        return Vk
  let botStr = show bot
  let botPath = "\\src\\Bot\\" <> botStr <> "\\" <> botStr <> ".json"
  botConfig <- set $ repDir <> botPath
  return $ HM.unions [botConfig, config]
