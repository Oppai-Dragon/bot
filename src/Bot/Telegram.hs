module Bot.Telegram
    ( getKeysTelegram
    , updateTelegram
    , updateKeys
    , getTelegramMsg
    ) where

import Bot
import Config
import Config.Get

import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T

import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class (lift)

type Message = T.Text

getKeysTelegram :: Object -> ReaderT Bot (StateT Config IO) Object
getKeysTelegram obj =
    let
        updateId = getValue ["update_id"] obj
        chatId = getValue ["message","chat","id"] obj
    in return $ HM.fromList [("offset",updateId),("chat_id",chatId)]

updateTelegram :: ReaderT Object (StateT Config IO) Message
updateTelegram = do
    updateKeys
    getTelegramMsg

updateKeys :: ReaderT Object (StateT Config IO) ()
updateKeys = do
    config <- lift $ get
    updates <- ask
    let updateIdOld = getValue ["offset"] config
    let updateIdNew = getValue ["update_id"] updates
    let chatId = getValue ["message","chat","id"] updates
    let isSendMsg = Bool $ (/=) updateIdOld updateIdNew
    let localConfig = HM.fromList [("offset",updateIdNew),("chat_id",chatId),("isSendMsg",isSendMsg)]
    lift $ put $ HM.union localConfig config

getTelegramMsg :: ReaderT Object (StateT Config IO) Message
getTelegramMsg = do
    updates <- ask
    let (String msg) = getValue ["message","text"] updates
    return msg