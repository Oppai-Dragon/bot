module Request.Modify
  ( modifyRequest
  , isNeedKeyboard
  , isNeedSticker
  , addKeyboard
  , addSticker
  ) where

import Base
import Config
import Config.Get
import Log

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE

import Control.Monad

import qualified Network.HTTP.Simple as HTTPSimple

modifyRequest :: ReqApp HTTPSimple.Request
modifyRequest = do
  addKeyboard
  addSticker
  getApp

isNeedKeyboard, isNeedSticker :: Config -> Bool
isNeedKeyboard conf =
  case HM.lookup "lastMsg" conf of
    Just (A.String "/repeat") -> True
    _ -> False

isNeedSticker conf =
  case HM.lookup "sticker_id" conf of
    Just (A.Number _) -> True
    _ -> False

addKeyboard, addSticker :: ReqApp ()
addKeyboard = do
  req <- getApp
  (Config.Handle config logHandle) <- liftApp getApp
  when (isNeedKeyboard config) $
    (>>) (liftApp . liftIO $ debugM logHandle "Add keyboard") . putApp $
    case getKeyboard config of
      (keybField, A.String keybValue):_ ->
        HTTPSimple.addToRequestQueryString
          [(TE.encodeUtf8 keybField, Just $ TE.encodeUtf8 keybValue)]
          req
      _ -> req

addSticker = do
  req <- getApp
  (Config.Handle config logHandle) <- liftApp getApp
  let stickerId = valueToInteger $ getValue ["sticker_id"] config
  let stickerIdBS = BS.pack $ show stickerId
  when (isNeedSticker config) $
    (>>)
      (liftApp . liftIO . debugM logHandle $ "Add sticker: " <> show stickerId) .
    putApp $
    (HTTPSimple.setRequestPath "/method/messages.sendSticker" .
     HTTPSimple.addToRequestQueryString [("sticker_id", Just stickerIdBS)])
      req
