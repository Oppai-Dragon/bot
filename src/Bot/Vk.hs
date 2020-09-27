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

type Attachment = A.Object

type TypeName = T.Text

type Message = T.Text

getKeys :: Updates -> App Updates
getKeys = return

update :: ObjApp Message
update = do
  Config.Handle {hLog=logHandle} <- liftApp getApp
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
  updates <- askSubApp
  let attachmentsObj = fromObject $ getValue ["object", "message"] updates
  let attachmentsObjArr =
        fromArrObject $ getValue ["attachments"] attachmentsObj
  liftApp $ handleAttachments attachmentsObjArr
  Config.Handle {hConfig=config,hLog=logHandle} <- liftApp getApp
  let attachments = getValue ["attachment"] config
  liftApp . liftIO . debugM logHandle $
    "Vk attachments: " <> (T.unpack . fromString) attachments


handleAttachments :: [Attachment] -> App ()
handleAttachments [] = return ()
handleAttachments (attachmentObj:rest) = do
  Config.Handle {hConfig=config,hLog=logHandle} <- getApp
  let typeName = fromString $ getValue ["type"] obj
  liftIO . debugM logHandle $ "Find vk attachments: " <> T.unpack typeName
  case typeName of
    "sticker" -> handleSticker attachmentObj
    "photo" -> handlePhoto attachmentObj
    "doc" -> handleDoc attachmentObj
    _ -> handleAnyAttachment attachmentObj
  handleAttachments rest

handleSticker, handlePhoto, handleDoc :: Attachment -> App ()
handleSticker attachmentObj =
  let typeName = "sticker"
      field = "sticker_id"
      value = getValue [typeName, field] attachmentObj
  in modifyConfig (HM.insert "attachment" (A.String typeName) . HM.insert field value)

handlePhoto attachmentObj = do
  configHandle <- getApp
  let sizesValue = getValue ["photo","sizes"] attachmentObj
  getPhotosRequest <- liftIO $ httpJSON =<< getPhotosGet configHandle


handleDoc = undefined
handleAnyAttachment :: T.Text -> Attachment -> App ()
handleAnyAttachment = undefined

getMsg :: ObjApp Message
getMsg = do
  updates <- askSubApp
  let msg = fromString $ getValue ["object", "message", "text"] updates
  return msg
