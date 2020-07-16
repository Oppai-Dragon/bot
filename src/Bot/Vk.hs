module Bot.Vk
    ( getKeysVk
    , updateVk
    , updateKeys
    , getVkMsg
    ) where

import Bot
import Config
import Config.Get

import           Data.Aeson
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T

import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class (lift)

type Message = T.Text

getKeysVk :: Object -> ReaderT Bot (StateT Config IO) Object
getKeysVk = return

updateVk :: ReaderT (Object,Object) (StateT Config IO) Message
updateVk = do
    (updates,responseObj) <- ask
    updateKeys
    lift $ runReaderT getVkMsg updates

updateKeys :: ReaderT (Object,Object) (StateT Config IO) ()
updateKeys = do
    config <- lift $ get
    (updates,response) <- ask
    let ts = getValue ["ts"] response
    let userId = getValue ["object","message","from_id"] updates
    let localConfig = HM.fromList [("ts",ts),("user_id",userId)]
    lift $ put $ HM.union localConfig config

getVkMsg :: ReaderT Object (StateT Config IO) Message
getVkMsg = do
    updates <- ask
    let msg = case getValue ["object","message","text"] updates of
            String text -> text
            _           -> ""
    return msg