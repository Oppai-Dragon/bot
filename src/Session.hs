module Session
  ( runBot
  ) where

import Base
import Config
import Config.Get
import Config.Update
import Log
import Request

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

type Updates = A.Object

runBot :: App ()
runBot = do
  handle@(Config.Handle _ logHandle) <- getApp
  bot <- liftIO $ getBot handle
  liftIO $ infoM logHandle $ show bot <> " bot is selected."
  json <- startRequest
  localConfig <- runSubApp (getKeys json) bot
  modifyConfig $ HM.union localConfig
  runSubApp liveSession bot
  liftIO $ endM logHandle

liveSession :: BotApp ()
liveSession = do
  (Config.Handle _ logHandle) <- liftApp getApp
  json <- liftApp askRequest
  updates <-
    case json of
      A.Object x -> getUpdates x
      _ -> do
        liftApp . liftIO $ warningM logHandle "Getted json isn't an object"
        return []
  if null updates
    then liveSession
    else echoMessage updates >> liveSession

echoMessage :: [Updates] -> BotApp ()
echoMessage (updates:rest) = do
  updates' <- liftApp $ checkUpdate updates
  if HM.null updates'
    then echoMessage rest
    else do
      bot <- askSubApp
      liftApp $ updateConfig updates bot
      (Config.Handle config logHandle) <- liftApp getApp
      let repeatN =
            case getValue ["repeatN"] config of
              A.Number n -> valueToInteger $ A.Number n
              _ -> 1
      liftApp . liftIO . debugM logHandle $
        "Number of repetitions " <> show repeatN <> "."
      liftApp $ sendMessage repeatN
      echoMessage rest
echoMessage _ = return ()

sendMessage :: Integer -> App ()
sendMessage 0 = return ()
sendMessage n = do
  updateRandomId
  sendRequest
  sendMessage (n - 1)
