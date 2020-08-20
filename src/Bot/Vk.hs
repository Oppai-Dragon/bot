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
import Config.Get

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type Response = A.Object

type Updates = A.Object

type Message = T.Text

getKeys :: Updates -> BotApp Updates
getKeys = return

update :: Response -> ObjApp Message
update response = do
  updateKeys response
  updateAttachments
  getMsg

updateKeys :: Response -> ObjApp ()
updateKeys response = do
  updates <- askApp
  let ts = getValue ["ts"] response
  let userId = getValue ["object", "message", "from_id"] updates
  let localConfig = HM.fromList [("ts", ts), ("user_id", userId)]
  fromApp . modifyConfig $ HM.union localConfig

getAttachment :: [Updates] -> T.Text
getAttachment [] = ""
getAttachment (obj:objs) =
  let typeName =
        case getValue ["type"] obj of
          A.String x -> x
          _ -> ""
      ownerIdText =
        T.pack . show . valueToInteger $ getValue [typeName, "owner_id"] obj
      objIdText = T.pack . show . valueToInteger $ getValue [typeName, "id"] obj
      accessKey =
        case getValue [typeName, "access_key"] obj of
          A.String text -> "_" <> text
          _ -> ""
      attachment = typeName <> ownerIdText <> "_" <> objIdText <> accessKey
   in "," <> attachment <> getAttachment objs

updateAttachments :: ObjApp ()
updateAttachments = do
  updates <- askApp
  let attachmentsObj =
        case getValue ["object", "message"] updates of
          A.Object x -> x
          _ -> HM.empty
  let attachments =
        case AT.parseMaybe (A..: "attachments") attachmentsObj :: Maybe [A.Object] of
          Just objArr ->
            A.String $
            case getAttachment objArr of
              "" -> ""
              text -> T.tail text
          Nothing -> A.String ""
  fromApp . modifyConfig $ HM.insert "attachment" attachments

getMsg :: ObjApp Message
getMsg = do
  updates <- askApp
  let msg =
        case getValue ["object", "message", "text"] updates of
          A.String text -> text
          _ -> ""
  return msg
