module Request.Modify
  ( modifyRequest
  , isNeedKeyboard
  , isNeedSticker
  , addKeyboard
  --, addVkSticker
  , addTelegramAttachment
  ) where

import Base
import Bot
import Config
import Config.Get
import Log

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Simple as HTTPSimple

modifyRequest :: ReqApp HTTPSimple.Request
modifyRequest = do
  addKeyboard
  configHandle <- liftApp getApp
  case hBot configHandle of
    Telegram -> addTelegramAttachment
    Vk -> addVkSticker
  getApp

isNeedKeyboard, isNeedSticker :: Config -> Bool
isNeedKeyboard conf =
  case HM.lookup "lastMsg" conf of
    Just (A.String "/repeat") -> True
    _ -> False

isNeedSticker conf =
  case HM.lookup "attachment" conf of
    Just (A.String "sticker") -> True
    _ -> False

addKeyboard, addVkSticker, addTelegramAttachment :: ReqApp ()
addKeyboard = do
  req <- getApp
  Config.Handle {hConfig = config, hLog = logHandle} <- liftApp getApp
  when (isNeedKeyboard config) $
    (>>) (liftApp . liftIO $ debugM logHandle "Add keyboard") . putApp $
    case getKeyboard config of
      (keybField, A.String keybValue):_ ->
        HTTPSimple.addToRequestQueryString
          [(TE.encodeUtf8 keybField, Just $ TE.encodeUtf8 keybValue)]
          req
      _ -> req

addVkSticker = do
  req <- getApp
  Config.Handle {hConfig = config} <- liftApp getApp
  let stickerIdBS = toBS $ getValue ["sticker_id"] config
  when (isNeedSticker config) . putApp $
    HTTPSimple.addToRequestQueryString [("sticker_id", Just stickerIdBS)] req

addTelegramAttachment = do
  req <- getApp
  Config.Handle {hConfig = config} <- liftApp getApp
  case fromString $ getValue ["method"] config of
    "Message" -> return ()
    attachment' -> do
      let fileId = fromString $ getValue ["file_id"] config
      let attachment = TE.encodeUtf8 $ T.toLower attachment'
      putApp $
        HTTPSimple.addToRequestQueryString
          [(attachment, Just $ TE.encodeUtf8 fileId)]
          req
