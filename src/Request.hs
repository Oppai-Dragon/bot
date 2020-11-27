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

maybeStartRequest :: App (Maybe A.Object)
maybeStartRequest = do
  configHandle <- getApp
  maybeReq <- liftIO $ getMaybeStartRequest configHandle
  case maybeReq of
    Nothing -> return Nothing
    Just req -> do
      maybeJson <- tryHttpJson $ HTTPSimple.httpJSON req
      handleJsonResponse maybeJson

maybeAskRequest :: App (Maybe A.Object)
maybeAskRequest = do
  configHandle <- getApp
  maybeReq <- liftIO $ getMaybeAskRequest configHandle
  case maybeReq of
    Nothing -> return Nothing
    Just req -> do
      maybeJson <- tryHttpJson $ HTTPSimple.httpJSON req
      handleJsonResponse maybeJson

sendRequest :: App ()
sendRequest = do
  configHandle <- getApp
  maybeReqDefault <- liftIO $ getMaybeSendRequest configHandle
  case maybeReqDefault of
    Nothing -> return ()
    Just reqDefault -> do
      req <- evalApp modifyRequest reqDefault
      maybeJson <- tryHttpJson $ HTTPSimple.httpJSON req
      void $ handleJsonResponse maybeJson
