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
  bot <- fromIO $ getBot handle
  fromIO $ infoM logHandle $ show bot <> " bot is selected."
  json <- startRequest
  localConfig <- runRApp (getKeys json) bot
  modifyConfig $ HM.union localConfig
  runRApp liveSession bot

liveSession :: BotApp ()
liveSession = do
  (Config.Handle _ logHandle) <- fromApp getApp
  json <- fromApp askRequest
  updates' <- fromApp $ unpackUpdates json
  updates <- fromApp $ checkUpdates updates'
  if HM.null updates
    then liveSession
    else (fromApp . fromIO) (debugM logHandle "The message is received") >>
         updateConfig updates json >>
         fromApp echoMessage >>
         liveSession

echoMessage :: App ()
echoMessage = do
  (Config.Handle config logHandle) <- getApp
  let repeatN =
        case getValue ["repeatN"] config of
          A.Number n -> valueToInteger $ A.Number n
          _ -> 1
  fromIO $ debugM logHandle $ "Number of repetitions " <> show repeatN <> "."
  sendMessage repeatN

sendMessage :: Integer -> App ()
sendMessage 0 = return ()
sendMessage n = do
  updateRandomId
  sendRequest
  sendMessage (n - 1)
