module Lib
    ( runBot
    ) where

import Bot
import Bot.Vk
import Bot.Telegram
import Config
import Config.Get

import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC
import qualified Data.Char              as C
import qualified Data.HashMap.Strict    as HM
import qualified Data.Vector            as V
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class (lift)

import Network.HTTP.Simple
import Network.HTTP.Client

type Message = T.Text

runBot :: StateT Config IO ()
runBot = do
    config <- get
    let request = getStartRequest config
    response <- lift $ httpJSON request
    let json = getResponseBody response
    let bot = getBot config
    localConfig <- runReaderT (getKeys json) bot
    put $ HM.union localConfig config
    liveSession
    return ()

liveSession :: StateT Config IO ()
liveSession = do
    config <- get
    let request = getAskRequest config
    response <- lift $ httpJSON request
    let json = getResponseBody response
    let bot = getBot config
    updates <- unpackUpdates json
    if HM.null updates
        then liveSession
        else runReaderT echoMessage (updates,bot)
    liveSession

getKeys :: Value -> ReaderT Bot (StateT Config IO) Object
getKeys (Object obj) = do
    bot <- ask
    config <- lift get
    let field = getUnpackField "start_request" config
    let responseObj = case HM.lookup field obj of
            Just (Object obj) -> obj
            _                 -> HM.empty
    case bot of
        Vk       -> getKeysVk responseObj
        Telegram -> getKeysTelegram responseObj
        _        -> return responseObj

echoMessage :: ReaderT (Object,Bot) (StateT Config IO) ()
echoMessage = do
    (updates,bot) <- ask
    updateConfig
    config <- lift $ get
    let repeatN = case getValue ["repeatN"] config of
            Number n -> read . takeWhile (/='.') $ show n
            _        -> 1
    lift $ sendMessage repeatN

updateConfig :: ReaderT (Object,Bot) (StateT Config IO) ()
updateConfig = do
    (updates,bot) <- ask
    msg <- case bot of
        Vk       -> updateVk
        Telegram -> updateTelegram
        _        -> return ""
    lift $ updateRepeatN msg
    lift $ msgHandler msg

updateRepeatN :: Message -> StateT Config IO ()
updateRepeatN msg = do
    config <- get
    let String lastMsg = getValue ["lastMsg"] config
    let msgStr = T.unpack msg
    let greatThenNull n = if n <= 0 then 1 else n
    let repeatN = Number $ case lastMsg of
            "/repeat" -> if and $ map C.isDigit msgStr
                then greatThenNull $ read msgStr
                else 1
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

sendMessage :: Int -> StateT Config IO ()
sendMessage 0 = return ()
sendMessage n = do
    config <- get
    let request = getSendRequest config
    let modifyReq req = case getValue ["lastMsg"] config of
            String "/repeat" -> addingKeyboard req config
            _                -> req
    httpBS $ modifyReq request
    sendMessage (n-1)

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
    let updateObj = case parseMaybe (.: field) obj :: Maybe [Object] of
            Just objArr -> HM.unions objArr
            _           -> HM.empty
    return updateObj