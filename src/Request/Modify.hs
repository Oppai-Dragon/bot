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
  Config.Handle {hConfig = config, hBot = bot} <- liftApp getApp
  when (isNeedKeyboard config) addKeyboard
  case bot of
    Telegram ->
      case fromString $ getValue ["method"] config of
        "Message" -> return ()
        attachmentUp -> addTelegramAttachment attachmentUp
    Vk -> when (isNeedSticker config) addVkSticker
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

addKeyboard, addVkSticker :: ReqApp ()
addKeyboard = do
  Config.Handle {hConfig = config, hLog = logHandle} <- liftApp getApp
  liftApp . liftIO $ debugM logHandle "Add keyboard"
  modifyReq $
    case getKeyboard config of
      (keybField, A.String keybValue):_ ->
        HTTPSimple.addToRequestQueryString
          [(TE.encodeUtf8 keybField, Just $ TE.encodeUtf8 keybValue)]
      _ -> id

addVkSticker = do
  Config.Handle {hConfig = config} <- liftApp getApp
  let stickerIdBS = toBS $ getValue ["sticker_id"] config
  modifyReq $
    HTTPSimple.addToRequestQueryString [("sticker_id", Just stickerIdBS)]

addTelegramAttachment :: T.Text -> ReqApp ()
addTelegramAttachment attachmentUp = do
  Config.Handle {hConfig = config} <- liftApp getApp
  let fileId = fromString $ getValue ["file_id"] config
  let attachmentLow = TE.encodeUtf8 $ T.toLower attachmentUp
  modifyReq $
    HTTPSimple.addToRequestQueryString
      [(attachmentLow, Just $ TE.encodeUtf8 fileId)]
