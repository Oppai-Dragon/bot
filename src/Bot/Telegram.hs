module Bot.Telegram
  ( getKeys
  , update
  , updateKeys
  , getMsg
  ) where

import Base
import Config

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type Message = T.Text

type Updates = A.Object

getKeys :: Updates -> BotApp Updates
getKeys obj =
  let chatId = getValue ["message", "chat", "id"] obj
   in return $ HM.fromList [("chat_id", chatId)]

update :: ObjApp Message
update = do
  updateKeys
  getMsg

updateKeys :: ObjApp ()
updateKeys = do
  updates <- askApp
  let updateId = getValue ["update_id"] updates
  let chatId = getValue ["message", "chat", "id"] updates
  let localConfig = HM.fromList [("offset", updateId), ("chat_id", chatId)]
  fromApp . modifyConfig $ HM.union localConfig

getMsg :: ObjApp Message
getMsg = do
  updates <- askApp
  let msg =
        case getValue ["message", "text"] updates of
          A.String x -> x
          _ -> ""
  return msg
