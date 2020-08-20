module Request.Modify
  ( modifyRequest
  , addKeyboard
  ) where

import Base
import Config
import Config.Get

import qualified Data.Aeson as A

import qualified Data.Text.Encoding as TE

import qualified Network.HTTP.Simple as HTTPSimple

modifyRequest :: ReqApp HTTPSimple.Request
modifyRequest = do
  addKeyboard
  getApp

addKeyboard :: ReqApp ()
addKeyboard = do
  req <- getApp
  (Config.Handle config _) <- fromApp getApp
  putApp $
    case getKeyboard config of
      (keybField, A.String keybValue):_ ->
        HTTPSimple.addToRequestQueryString
          [(TE.encodeUtf8 keybField, Just $ TE.encodeUtf8 keybValue)]
          req
      _ -> req
