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
  (Config.Handle config _) <- getApp
  let request = getStartRequest config
  fromIO $ infoM "Session:26" "Start Request"
  response <- fromIO $ HTTPSimple.httpJSON request
  fromIO $ infoM " " "Success"
  let json = HTTPSimple.getResponseBody response
  return json

askRequest :: A.FromJSON a => App a
askRequest = do
  (Config.Handle config _) <- getApp
  let request = getAskRequest config
  fromIO $ debugM "Session:74 " "Ask Request"
  response <- fromIO $ HTTPSimple.httpJSON request
  fromIO $ debugM " " "Success"
  let json = HTTPSimple.getResponseBody response
  return json

sendRequest :: App ()
sendRequest = do
  (Config.Handle config _) <- getApp
  let reqDefault = getSendRequest config
  request <- runSApp modifyRequest reqDefault
  fromIO $ print request
  fromIO $ debugM "Session:61" "Send Request"
  _ <- fromIO $ HTTPSimple.httpBS request
  fromIO $ debugM " " "Success"
