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
  (Config.Handle config _) <- getApp
  let bot = getBot config
  fromIO $ infoM " " $ show bot <> " bot is selected."
  json <- startRequest
  localConfig <- runRApp (getKeys json) bot
  modifyConfig $ HM.union localConfig
  liveSession

liveSession :: App ()
liveSession = do
  (Config.Handle config _) <- getApp
  json <- askRequest
  let bot = getBot config
  updates' <- unpackUpdates json
  updates <- checkUpdates updates'
  if HM.null updates
    then liveSession
    else fromIO (debugM "Session:44" "The message is received.") >>
         runRApp (updateConfig updates json) bot >>
         echoMessage

echoMessage :: App ()
echoMessage = do
  (Config.Handle config _) <- getApp
  let repeatN =
        case getValue ["repeatN"] config of
          A.Number n -> valueToInteger $ A.Number n
          _ -> 1
  fromIO $ debugM "Lig:51" $ "Number of repetitions " <> show repeatN <> "."
  sendMessage repeatN

sendMessage :: Integer -> App ()
sendMessage 0 = liveSession
sendMessage n = do
  updateRandomId
  sendRequest
  sendMessage (n - 1)
