{-# LANGUAGE LambdaCase #-}

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
import qualified Data.Text as T

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
  let bot = case getValue ["bot"] config of
        A.String name -> name
        _ -> ""
  let maybeLevel = AT.parseMaybe A.parseJSON (A.Object config)
  let logHandle = Handle logPath maybeLevel
  let errorLog = const $ errorM logHandle "Can't find bot implementation, check his name in Config.json"
  let infoLog = const $ infoM logHandle "Bot implementation is found"
  result <- tryM (return . read $ T.unpack bot :: IO Bot)
  either infoLog errorLog result
  let botPath = T.unpack $ "\\src\\Bot\\" <> bot <> "\\" <> bot <> ".json"
  botConfig <- set $ repDir <> botPath
  return $ HM.unions [botConfig, config]
