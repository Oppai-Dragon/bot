module Session
  ( runBot
  ) where

import Base
import Config
import Config.Get
import Helpers
import Log.Console

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

import qualified Network.HTTP.Simple as HTTPSimple

runBot :: App ()
runBot = do
  (Config.Handle config _) <- getApp
  let bot = getBot config
  fromIO $ infoM " " $ show bot <> " bot is selected."
  let request = getStartRequest config
  fromIO $ infoM "Session:26" "Start Request"
  response <- fromIO $ HTTPSimple.httpJSON request
  fromIO $ infoM " " "Success"
  let json = HTTPSimple.getResponseBody response
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
  (Config.Handle config _) <- getApp
  let reqDefault = getSendRequest config
  request <- runSApp modifyRequest reqDefault
  fromIO $ print request
  fromIO $ debugM "Session:61" "Send Request"
  _ <- fromIO $ HTTPSimple.httpBS request
  fromIO $ debugM " " "Success"
  sendMessage (n - 1)

modifyRequest :: ReqApp HTTPSimple.Request
modifyRequest = do
  addKeyboard
  getApp

askRequest :: A.FromJSON a => App a
askRequest = do
  (Config.Handle config _) <- getApp
  let request = getAskRequest config
  fromIO $ debugM "Session:74 " "Ask Request"
  response <- fromIO $ HTTPSimple.httpJSON request
  fromIO $ debugM " " "Success"
  json <- HTTPSimple.getResponseBody response
  return json
