module Bot.Telegram
  ( getKeysTelegram
  , updateTelegram
  , updateKeys
  , getTelegramMsg
  ) where

import Bot
import Config
import Config.Get

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

type Message = T.Text

getKeysTelegram :: Object -> ReaderT Bot (StateT Config IO) Object
getKeysTelegram obj =
  let chatId = getValue ["message", "chat", "id"] obj
   in return $ HM.fromList [("chat_id", chatId)]

updateTelegram :: ReaderT Object (StateT Config IO) Message
updateTelegram = do
  updateKeys
  getTelegramMsg

updateKeys :: ReaderT Object (StateT Config IO) ()
updateKeys = do
  updates <- ask
  let updateId = getValue ["update_id"] updates
  let chatId = getValue ["message", "chat", "id"] updates
  let localConfig = HM.fromList [("offset", updateId), ("chat_id", chatId)]
  lift . modify $ HM.union localConfig

getTelegramMsg :: ReaderT Object (StateT Config IO) Message
getTelegramMsg = do
  updates <- ask
  let (String msg) = getValue ["message", "text"] updates
  return msg
