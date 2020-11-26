module Request
  ( maybeStartRequest
  , maybeAskRequest
  , sendRequest
  ) where

import Base
import Config
import Config.Get
import Request.Exception
import Request.Modify

import Control.Monad (void)
import qualified Data.Aeson as A
import qualified Network.HTTP.Simple as HTTPSimple

maybeStartRequest :: App A.Object
maybeStartRequest = do
  configHandle <- getApp
  request <- liftIO $ getStartRequest configHandle
  json <- tryHttpJson $ HTTPSimple.httpJSON request
  handleJsonResponse json

maybeAskRequest :: App A.Object
maybeAskRequest = do
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
