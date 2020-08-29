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

runBot :: App ()
runBot = do
  handle@(Config.Handle _ logHandle) <- getApp
  bot <- liftIO $ getBot handle
  liftIO $ infoM logHandle $ show bot <> " bot is selected."
  json <- startRequest
  localConfig <- runSubApp (getKeys json) bot
  modifyConfig $ HM.union localConfig
  runSubApp liveSession bot

liveSession :: BotApp ()
liveSession = do
  (Config.Handle _ logHandle) <- liftApp getApp
  json <- liftApp askRequest
  updates' <- liftApp $ unpackUpdates json
  updates <- liftApp $ checkUpdates updates'
  if HM.null updates
    then liveSession
    else (liftApp . liftIO) (debugM logHandle "The message is received") >>
         updateConfig updates json >>
         liftApp echoMessage >>
         liveSession

echoMessage :: App ()
echoMessage = do
  (Config.Handle config logHandle) <- getApp
  let repeatN =
        case getValue ["repeatN"] config of
          A.Number n -> valueToInteger $ A.Number n
          _ -> 1
  liftIO . debugM logHandle $ "Number of repetitions " <> show repeatN <> "."
  sendMessage repeatN

sendMessage :: Integer -> App ()
sendMessage 0 = return ()
sendMessage n = do
  updateRandomId
  sendRequest
  sendMessage (n - 1)
