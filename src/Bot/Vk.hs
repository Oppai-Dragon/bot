module Bot.Vk
    ( getKeysVk
    , updateVk
    , updateKeys
    , getAttachment
    , updateAttachments
    , getVkMsg
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

getKeysVk :: Object -> ReaderT Bot (StateT Config IO) Object
getKeysVk = return

updateVk :: ReaderT (Object,Object) (StateT Config IO) Message
updateVk = do
    (updates,responseObj) <- ask
    updateKeys
    lift $ runReaderT updateAttachments updates
    lift $ runReaderT getVkMsg updates

updateKeys :: ReaderT (Object,Object) (StateT Config IO) ()
updateKeys = do
    config <- lift $ get
    (updates,response) <- ask
    let ts = getValue ["ts"] response
    let userId = getValue ["object","message","from_id"] updates
    let localConfig = HM.fromList [("ts",ts),("user_id",userId)]
    lift $ put $ HM.union localConfig config

getAttachment :: [Object] -> T.Text
getAttachment []         = ""
getAttachment (obj:objs) =
    let
        String typeName = getValue ["type"] obj
        ownerIdText = T.pack . show . valueToInteger $ getValue [typeName,"owner_id"] obj
        objIdText = T.pack . show . valueToInteger $ getValue [typeName,"id"] obj
        accessKey = case getValue [typeName,"access_key"] obj of
            String text -> "_" <> text
            _           -> ""
        attachment = typeName <> ownerIdText <> "_" <> objIdText <> accessKey
    in "," <> attachment <> getAttachment objs

updateAttachments :: ReaderT Object (StateT Config IO) ()
updateAttachments = do
    updates <- ask
    config <- lift get
    let Object attachmentsObj = getValue ["object","message"] updates
    let attachments =
            case parseMaybe (.: "attachments") attachmentsObj :: Maybe [Object] of
                Just objArr -> String $
                    case getAttachment objArr of
                        ""      -> ""
                        text    -> T.tail text
                Nothing     -> String ""
    lift . put $ HM.insert "attachment" attachments config

getVkMsg :: ReaderT Object (StateT Config IO) Message
getVkMsg = do
    updates <- ask
    let msg = case getValue ["object","message","text"] updates of
            String text -> text
            _           -> ""
    return msg