module Request
  ( startRequest
  , askRequest
  , sendRequest
  ) where

import Base
import Config
import Config.Get
import Log
import Request.Exception
import Request.Modify

import Control.Monad
import qualified Data.Aeson as A
import qualified Network.HTTP.Simple as HTTPSimple

startRequest :: App A.Object
startRequest = do
  configHandle <- getApp
  request <- liftIO $ getStartRequest configHandle
  json <- tryHttpJson $ HTTPSimple.httpJSON request
  handleJsonResponse json

askRequest :: App A.Object
askRequest = do
  configHandle <- getApp
  request <- liftIO $ getAskRequest configHandle
  json <- tryHttpJson $ HTTPSimple.httpJSON request
  handleJsonResponse json

sendRequest :: App ()
sendRequest = do
  configHandle <- getApp
  reqDefault <- liftIO $ getSendRequest configHandle
  request <- evalApp modifyRequest reqDefault
  json <- tryHttpJson $ HTTPSimple.httpJSON request
  void $ handleJsonResponse json
