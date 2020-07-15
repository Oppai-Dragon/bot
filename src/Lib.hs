module Lib
    ( runBot
    ) where

import Bot
import Config
import Config.Get

import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC
import qualified Data.HashMap.Strict    as HM
import qualified Data.Vector            as V
import qualified Data.Text.Encoding     as TE

import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class (lift)

import Network.HTTP.Simple
import Network.HTTP.Client

runBot :: StateT Config IO ()
runBot = do
    config <- get
    let request = getStartRequest config
    response <- lift $ httpJSON request
    let json = getResponseBody response
    localConfig <- getKeys json
    put $ HM.union localConfig config
    liveSession
    return ()

liveSession :: StateT Config IO ()
liveSession = do
    config <- get
    let request = getAskRequest config
    response <- lift $ httpJSON request
    let json = getResponseBody response
    let updates = unpackUpdates json
    updateTs json
    if HM.null updates
        then liveSession
        else runReaderT echoMessage updates
    liveSession

addingKeyboard :: Request -> Config -> Request
addingKeyboard req conf =
    case getKeyboard conf of
        (keybField,String keybValue):rest ->
            addToRequestQueryString
            [(TE.encodeUtf8 keybField, Just $ TE.encodeUtf8 keybValue)]
            req
        []                                -> req

updateTs :: Value -> StateT Config IO ()
updateTs (Object obj) =
    let newTs = getValue "ts" obj
    in get >>= put . HM.insert "ts" newTs

getKeys :: Value -> StateT Config IO Object
getKeys (Object obj) = do
    config <- get
    let field = getUnpackField "start_request" config
    let responseObj = case HM.lookup field obj of
            Just (Object obj) -> obj
            _                 -> HM.empty
    return responseObj

echoMessage :: ReaderT Object (StateT Config IO) ()
echoMessage = do
    updates <- ask
    let (Object messageObj) = parseFieldsFunc
            ["object","message"] (Object updates)
    let userId = getInteger "from_id" messageObj
    let messageGet = getText "text" messageObj
    config <- lift get
    return ()

sendMessage :: ReaderT Object (StateT Config IO) ()
sendMessage = do
    updates <- ask
    config <- lift get
    return ()

unpackUpdates :: Value -> StateT Config IO Object
unpackUpdates (Object obj) =
    config <- get
    let field = getUnpackField "ask_request" config
    let updateObj = case parseMaybe (.: field) obj :: Maybe [Object] of
        Just objArr -> HM.unions objArr
        _           -> HM.empty