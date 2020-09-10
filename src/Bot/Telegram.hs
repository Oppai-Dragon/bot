module Bot.Telegram
  ( getKeys
  , update
  , updateKeys
  , getMsg
  ) where

import Base
import Config
import Log

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Char
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T

type Message = T.Text

type Updates = A.Object

getKeys :: Updates -> BotApp Updates
getKeys obj =
  let chatId = getValue ["message", "chat", "id"] obj
   in return $ HM.fromList [("chat_id", chatId)]

update :: ObjApp Message
update = do
  (Config.Handle _ logHandle) <- liftApp getApp
  updates <- askSubApp
  liftApp . liftIO . infoM logHandle $ "Updates telegram: " <> show updates
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
  (Config.Handle config logHandle) <- liftApp getApp
  let ignoredArr =
        fromMaybe [] . AT.parseMaybe A.parseJSON $ getValue ["ignore"] config :: [T.Text]
  let messageObj = fromObj $ getValue ["message"] updates
  let restUpdates = deleteIgnore ignoredArr messageObj
  let attachHandle attach =
        liftApp $ do
          liftIO . infoM logHandle $ "Telegram attachments: " <> show attach
          let method = (toUpper . T.head) attach `T.cons` T.tail attach
          let fileId = getValue [attach, "file_id"] restUpdates
          liftIO . infoM logHandle $
            "method - send" <> T.unpack method <> ", file_id - " <> show fileId
          let localConfig =
                HM.fromList [("method", A.String method), ("file_id", fileId)]
          modifyConfig $ HM.union localConfig
  case getValue ["text"] messageObj of
    A.String _ ->
      liftApp . modifyConfig . HM.insert "method" $ A.String "Message"
    _ ->
      case HM.keys restUpdates of
        [x] -> attachHandle x
        arr ->
          case findText "document" arr of
            Just x -> attachHandle x
            Nothing -> return ()

deleteIgnore :: [T.Text] -> Updates -> Updates
deleteIgnore [] = id
deleteIgnore (field:rest) = HM.delete field . deleteIgnore rest

getMsg :: ObjApp Message
getMsg = do
  updates <- askSubApp
  let messageObj = fromObj $ getValue ["message"] updates
  let msg =
        case getValue ["text"] messageObj of
          A.String x -> x
          _ -> ""
  return msg

fromObj :: A.Value -> A.Object
fromObj value =
  case value of
    A.Object x -> x
    _ -> HM.empty
