module Request
  ( startRequest
  , askRequest
  , sendRequest
  ) where

import Base
import Config
import Config.Get
import Log
import Request.Modify

import qualified Data.Aeson as A

import qualified Network.HTTP.Simple as HTTPSimple

startRequest :: A.FromJSON a => App a
startRequest = do
  (Config.Handle config logHandle) <- getApp
  let request = getStartRequest config
  liftIO $ infoM logHandle "Start Request"
  responseJSON request

askRequest :: A.FromJSON a => App a
askRequest = do
  (Config.Handle config logHandle) <- getApp
  let request = getAskRequest config
  liftIO $ debugM logHandle "Ask Request"
  responseJSON request

sendRequest :: App ()
sendRequest = do
  (Config.Handle config logHandle) <- getApp
  let reqDefault = getSendRequest config
  request <- evalApp modifyRequest reqDefault
  liftIO $ debugM logHandle "Send Request"
  _ <- liftIO $ HTTPSimple.httpBS request
  liftIO $ infoM logHandle "Message sended"

responseJSON :: A.FromJSON a => HTTPSimple.Request -> App a
responseJSON req = do
  (Config.Handle _ logHandle) <- getApp
  response <- liftIO $ HTTPSimple.httpJSON req
  liftIO $ infoM logHandle "Response getted"
  let json = HTTPSimple.getResponseBody response
  return json
