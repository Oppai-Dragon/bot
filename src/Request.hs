module Request
  ( maybeTStartRequest
  , maybeTAskRequest
  , maybeTSendRequest
  ) where

import Base
import Config
import Config.Get
import Request.Exception
import Request.Modify

import Control.Monad.Trans.Maybe
import Control.Monad (void)
import qualified Data.Aeson as A
import qualified Network.HTTP.Simple as HTTPSimple

maybeTStartRequest :: MaybeT App A.Object
maybeTStartRequest = maybeTReqWithObject getMaybeTStartRequest

maybeTAskRequest :: MaybeT App A.Object
maybeTAskRequest = maybeTReqWithObject getMaybeTAskRequest

maybeTReqWithObject :: (Config.Handle -> MaybeT IO HTTPSimple.Request) -> MaybeT App A.Object
maybeTReqWithObject getMaybeTReqFunc = do
  configHandle <- liftApp getApp
  req <- liftMaybeT $ getMaybeTReqFunc configHandle
  json <- tryHttpJson $ HTTPSimple.httpJSON req
  handleJsonResponse json

maybeTSendRequest :: MaybeT App ()
maybeTSendRequest = do
  configHandle <- liftApp getApp
  reqDefault <- liftMaybeT $ getMaybeTSendRequest configHandle
  req <- liftApp $ evalApp modifyRequest reqDefault
  json <- tryHttpJson $ HTTPSimple.httpJSON req
  void $ handleJsonResponse json
