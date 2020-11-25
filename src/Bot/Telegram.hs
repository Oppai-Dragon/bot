module Bot.Telegram
  ( getKeys
  , update
  , updateKeys
  , updateMethod
  , handleAttachment
  , getMsg
  ) where

import Base
import Config
import Log

import qualified Data.Aeson as A
import Data.Char
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type Message = T.Text

type Updates = A.Object

getKeys :: Updates -> App Updates
getKeys obj =
  let chatId = getValue ["message", "chat", "id"] obj
   in return $ HM.fromList [("chat_id", chatId)]

update :: ObjApp Message
update = do
  Config.Handle {hLog = logHandle} <- liftApp getApp
  updates <- askSubApp
  liftApp . liftIO . logInfo logHandle $ "Updates telegram: " <> show updates
  updateKeys
  updateMethod
  getMsg

updateKeys :: ObjApp ()
updateKeys = do
  updates <- askSubApp
  let updateId = getValue ["update_id"] updates
  let chatId = getValue ["message", "chat", "id"] updates
  let localConfig = HM.fromList [("offset", updateId), ("chat_id", chatId)]
  liftApp . modifyConfig $ HM.union localConfig

updateMethod :: ObjApp ()
updateMethod = do
  updates <- askSubApp
  Config.Handle {hConfig = config} <- liftApp getApp
  let attachmentArr = fromArrString $ getValue ["attachments"] config
  let messageObj = fromObject $ getValue ["message"] updates
  case getValue ["text"] messageObj of
    A.String _ ->
      liftApp . modifyConfig . HM.insert "method" $ A.String "Message"
    _ ->
      case findValue attachmentArr messageObj of
        Just (attachment, A.Object obj) ->
          liftApp $ handleAttachment attachment obj
        Just (attachment, value@(A.Array _)) ->
          liftApp $ handleAttachment attachment (head $ fromArrObject value)
        _ -> return ()

handleAttachment :: T.Text -> A.Object -> App ()
handleAttachment attachment attachmentObj = do
  Config.Handle {hLog = logHandle} <- getApp
  liftIO . logInfo logHandle $ "Telegram attachments: " <> T.unpack attachment
  let method = (toUpper . T.head) attachment `T.cons` T.tail attachment
  let fileId = getValue ["file_id"] attachmentObj
  liftIO . logInfo logHandle $
    "method - send" <> T.unpack method <> ", file_id - " <> show fileId
  let localConfig =
        HM.fromList [("method", A.String method), ("file_id", fileId)]
  liftIO . logDebug logHandle $ "Telegram method is : send" <> T.unpack method
  modifyConfig $ HM.union localConfig

getMsg :: ObjApp Message
getMsg = do
  updates <- askSubApp
  let messageObj = fromObject $ getValue ["message"] updates
  let msg = fromString $ getValue ["text"] messageObj
  return msg
