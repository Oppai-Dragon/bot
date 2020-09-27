module Request
  ( startRequest
  , askRequest
  , sendRequest
  ) where

import Base
import Config
import Config.Get
import Request.Exception
import Request.Modify

import Control.Monad
import qualified Data.Aeson as A
import qualified Network.HTTP.Simple as HTTPSimple

startRequest :: App A.Value
startRequest = do
  configHandle <- getApp
  request <- liftIO $ getStartRequest configHandle
  tryHttpJson $ HTTPSimple.httpJSON request

askRequest :: App A.Value
askRequest = do
  configHandle <- getApp
  request <- liftIO $ getAskRequest configHandle
  tryHttpJson $ HTTPSimple.httpJSON request

sendRequest :: App ()
sendRequest = do
  configHandle <- getApp
  reqDefault <- liftIO $ getSendRequest configHandle
  request <- evalApp modifyRequest reqDefault
  void . tryHttpJson $ HTTPSimple.httpJSON request
