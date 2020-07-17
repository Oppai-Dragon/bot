module Helpers
    ( getKeys
    , getLastObj
    , checkUpdates
    , updateConfig
    , updateRepeatN
    , msgHandler
    , updateRandomId
    , addingKeyboard
    , unpackUpdates
    ) where

import Bot
import Bot.Vk
import Bot.Telegram
import Config
import Config.Get

import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.Char              as C

import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class (lift)

import Network.HTTP.Client (Request)
import Network.HTTP.Simple (addToRequestQueryString)

type Message = T.Text

getKeys :: Value -> ReaderT Bot (StateT Config IO) Object
getKeys (Object obj) = do
    bot <- ask
    config <- lift get
    let field = getUnpackField "start_request" config
    let updateObj = case parseMaybe (.: field) obj of
            Just (Array vector)       -> getLastObj (Array vector)
            Just (Object responseObj) -> responseObj
            _                         -> HM.empty
    case bot of
        Vk       -> getKeysVk updateObj
        Telegram -> getKeysTelegram updateObj
        _        -> return updateObj

getLastObj :: Value -> Object
getLastObj value =
    let arrayObj = HM.singleton "array" value
    in case parseMaybe (.: "array") arrayObj :: Maybe [Object] of
        Just objArr ->
            if null objArr
                then HM.empty
                else last objArr
        Nothing     -> HM.empty

checkUpdates :: Object -> StateT Config IO Object
checkUpdates updates = do
    config <- get
    let updateIdOld = getValue ["offset"] config
    case HM.lookup "update_id" updates of
        Just value -> if updateIdOld == value
            then return HM.empty
            else return updates
        Nothing    -> return updates

updateConfig :: ReaderT ((Object,Value),Bot) (StateT Config IO) ()
updateConfig = do
    ((updates,Object response),bot) <- ask
    msg <- lift $ case bot of
        Vk       -> runReaderT updateVk (updates,response)
        Telegram -> runReaderT updateTelegram updates
        _        -> return ""
    lift $ updateRepeatN msg
    lift $ msgHandler msg

updateRepeatN :: Message -> StateT Config IO ()
updateRepeatN msg = do
    config <- get
    let lastMsg = getValue ["lastMsg"] config
    let msgStr = T.unpack msg
    let greatThenNull n = if n <= 0 then 1 else n
    let oldRepeatN = getValue ["repeatN"] config
    let repeatN = case lastMsg of
            String "/repeat" -> Number $
                if and $ map C.isDigit msgStr
                    then greatThenNull $ read msgStr
                    else 1
            _                -> oldRepeatN
    put $ HM.insert "repeatN" repeatN config

msgHandler :: Message -> StateT Config IO ()
msgHandler msg = do
    config <- get
    let (String msgField) = getValue ["msgField"] config
    let getMsg x = getValue [x] config
    let msgObj = HM.singleton msgField $
            case msg of
                "/help"   -> getValue ["helpMsg"] config
                "/repeat" -> getRepeatMsg config
                _         -> String msg
    let localConfig = HM.singleton "lastMsg" (String msg) `HM.union` msgObj
    put $ HM.union localConfig config

updateRandomId :: StateT Config IO ()
updateRandomId = do
    randomN <- lift $ getRandom
    let randomId = Number . read $ show randomN
    config <- get
    put $ HM.insert "random_id" randomId config

addingKeyboard :: Request -> Config -> Request
addingKeyboard req conf =
    case getKeyboard conf of
        (keybField,String keybValue):rest ->
            addToRequestQueryString
            [(TE.encodeUtf8 keybField, Just $ TE.encodeUtf8 keybValue)]
            req
        []                                -> req

unpackUpdates :: Value -> StateT Config IO Object
unpackUpdates (Object obj) = do
    config <- get
    let field = getUnpackField "ask_request" config
    let updateObj = case parseMaybe (.: field) obj of
            Just (Array vector)       -> getLastObj (Array vector)
            Just (Object responseObj) -> responseObj
            _                         -> HM.empty
    return updateObj