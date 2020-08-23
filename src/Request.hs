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
  fromIO $ infoM logHandle "Start Request"
  response <- fromIO $ HTTPSimple.httpJSON request
  fromIO $ infoM logHandle "Response getted"
  let json = HTTPSimple.getResponseBody response
  return json

askRequest :: A.FromJSON a => App a
askRequest = do
  (Config.Handle config logHandle) <- getApp
  let request = getAskRequest config
  fromIO $ debugM logHandle "Ask Request"
  response <- fromIO $ HTTPSimple.httpJSON request
  fromIO $ infoM logHandle "Response getted"
  let json = HTTPSimple.getResponseBody response
  return json

sendRequest :: App ()
sendRequest = do
  (Config.Handle config logHandle) <- getApp
  let reqDefault = getSendRequest config
  request <- runSApp modifyRequest reqDefault
  fromIO $ debugM logHandle "Send Request"
  _ <- fromIO $ HTTPSimple.httpBS request
  fromIO $ infoM logHandle "Message sended"
