module Request.Modify
  ( modifyRequest
  , addKeyboard
  ) where

import Base
import Config
import Config.Get
import Log

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE

import qualified Network.HTTP.Simple as HTTPSimple

modifyRequest :: ReqApp HTTPSimple.Request
modifyRequest = do
  addKeyboard
  getApp

isNeedKeyboard :: Config -> Bool
isNeedKeyboard conf =
  case HM.lookup "lastMsg" conf of
    Just "/repeat" -> True
    _ -> False

addKeyboard :: ReqApp ()
addKeyboard = do
  req <- getApp
  (Config.Handle config logHandle) <- fromApp getApp
  if isNeedKeyboard config
    then (>>) (fromApp . fromIO $ debugM logHandle "Add keyboard") . putApp $
         case getKeyboard config of
           (keybField, A.String keybValue):_ ->
             HTTPSimple.addToRequestQueryString
               [(TE.encodeUtf8 keybField, Just $ TE.encodeUtf8 keybValue)]
               req
           _ -> req
    else return ()
