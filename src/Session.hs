module Session
  ( runBot
  ) where

import Base
import Bot
import Config
import Config.Update
import Log
import Request

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

type Updates = A.Object

runBot :: App ()
runBot = do
  Config.Handle {hLog = logHandle, hBot = bot} <- getApp
  liftIO $ infoM logHandle $ show bot <> " bot is selected."
  obj <- startRequest
  localConfig <- getKeys obj
  modifyConfig $ HM.union localConfig
  liveSession
  liftIO $ endM logHandle

liveSession :: App ()
liveSession = do
  obj <- askRequest
  updates <- getUpdates obj
  if null updates
    then liveSession
    else echoMessage updates >> liveSession

echoMessage :: [Updates] -> App ()
echoMessage (updates:rest) = do
  Config.Handle {hConfig = config, hLog = logHandle, hBot = bot} <- getApp
  updates' <-
    case bot of
      Vk -> return updates
      Telegram -> checkUpdate updates
  if HM.null updates'
    then echoMessage rest
    else do
      updateConfig updates bot
      let repeatN = Base.toInteger $ getValue ["repeatN"] config
      liftIO . debugM logHandle $
        "Number of repetitions " <> show repeatN <> "."
      sendMessage repeatN
      echoMessage rest
echoMessage _ = return ()

sendMessage :: Integer -> App ()
sendMessage 0 = return ()
sendMessage n = do
  updateRandomId
  sendRequest
  sendMessage (n - 1)
