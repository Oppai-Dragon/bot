module Session
  ( runBot
  ) where

import Base
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
  json <- startRequest
  localConfig <- getKeys json
  modifyConfig $ HM.union localConfig
  liveSession
  liftIO $ endM logHandle

liveSession :: App ()
liveSession = do
  Config.Handle {hLog = logHandle} <- getApp
  json <- askRequest
  updates <-
    case json of
      A.Object x -> getUpdates x
      _ -> do
        liftIO $ warningM logHandle "Getted json isn't an object"
        return []
  if null updates
    then liveSession
    else echoMessage updates >> liveSession

echoMessage :: [Updates] -> App ()
echoMessage (updates:rest) = do
  updates' <- checkUpdate updates
  Config.Handle {hConfig = config, hLog = logHandle, hBot = bot} <- getApp
  if HM.null updates'
    then echoMessage rest
    else do
      updateConfig updates bot
      let repeatValue = getValue ["repeatN"] config
      let repeatN =
            case repeatValue of
              A.Number _ -> Base.toInteger repeatValue
              _ -> 1
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
