module Bot.Vk
  ( getKeys
  , update
  , updateKeys
  , getAttachment
  , updateAttachments
  , getMsg
  ) where

import Base
import Config
import Log

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T

type Updates = A.Object

type Message = T.Text

getKeys :: Updates -> BotApp Updates
getKeys = return

update :: ObjApp Message
update = do
  (Config.Handle _ logHandle) <- liftApp getApp
  updates <- askSubApp
  liftApp . liftIO . infoM logHandle $ "Updates vk: " <> show updates
  updatePeerId
  updateAttachments
  getMsg

updatePeerId :: ObjApp ()
updatePeerId = do
  updates <- askSubApp
  let userId = getValue ["object", "message", "peer_id"] updates
  liftApp . modifyConfig $ HM.insert "peer_id" userId

updateKeys :: Updates -> App ()
updateKeys updates =
  let key = "ts"
      ts = getValue [key] updates
   in modifyConfig $ HM.insert key ts

getAttachment :: [Updates] -> T.Text
getAttachment [] = ""
getAttachment (obj:objs) =
  let typeName = fromString $ getValue ["type"] obj
      ownerIdText =
        T.pack . show . valueToInteger $ getValue [typeName, "owner_id"] obj
      objIdText = T.pack . show . valueToInteger $ getValue [typeName, "id"] obj
      accessKey =
        case fromString $ getValue [typeName, "access_key"] obj of
          "" -> ""
          text -> "_" <> text
      attachment = typeName <> ownerIdText <> "_" <> objIdText <> accessKey
   in "," <> attachment <> getAttachment objs

updateAttachments :: ObjApp ()
updateAttachments = do
  (Config.Handle _ logHandle) <- liftApp getApp
  updates <- askSubApp
  let attachmentsObj = fromObject $ getValue ["object", "message"] updates
  let attachmentsObjArr =
        fromMaybe
          []
          (AT.parseMaybe (A..: "attachments") attachmentsObj :: Maybe [A.Object])
  let attachments =
        A.String $
        if null attachmentsObjArr
          then ""
          else case getAttachment attachmentsObjArr of
                 "" -> ""
                 text -> T.tail text
  let stickerId =
        if null attachmentsObjArr
          then A.Null
          else getValue ["sticker", "sticker_id"] $ head attachmentsObjArr
  liftApp . liftIO . debugM logHandle $
    "Vk attachments: " <> (\(A.String x) -> T.unpack x) attachments
  liftApp . modifyConfig $ HM.insert "sticker_id" stickerId
  liftApp . modifyConfig $ HM.insert "attachment" attachments

getMsg :: ObjApp Message
getMsg = do
  updates <- askSubApp
  let msg = fromString $ getValue ["object", "message", "text"] updates
  return msg
