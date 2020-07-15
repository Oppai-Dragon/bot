module Lib
    ( runBot
    ) where

import Config
import Config.Get

import           Data.Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC
import qualified Data.Char              as C
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text.Encoding     as TE

import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class (lift)

import Network.HTTP.Simple
import Network.HTTP.Client

data Bot = Vk | Telegram deriving (Show,Eq)
instance Read Bot where
    readPrec _ input =
        case map C.toUpper input of
            "VK" -> VK
            "TELEGRAM" -> Telegram

runBot :: StateT Config IO ()
runBot = do
    config <- get
    let request = getStartRequest config
    response <- lift $ httpJSON request
    let json = getResponseBody response
    localConfig <- runReaderT (getKeys json) getBot
    put $ HM.union localConfig config
    liveSession
    return ()

liveSession :: StateT Config IO ()
liveSession = do
    config <- get
    response <- lift $ askRequest config
    lift $ print response
    let json = getResponseBody response
    let updates = unpackUpdates json
    if HM.null updates
        then liveSession
        else sendRequest
    liveSession

addingKeyboard :: Request -> Config -> Request
addingKeyboard req conf =
    case getKeyboard conf of
        (keybField,String keybValue):rest ->
            addToRequestQueryString
            [(TE.encodeUtf8 keybField, Just $ TE.encodeUtf8 keybValue)]
            req
        []                                -> req

getKeys :: Value -> ReaderT Bot (StateT Config IO) Object
getKeys (Object obj) = do
    bot <- ask
    obj <- lift $ if bot == Vk
        then getKeysVK obj
        else getKeysTelegram obj
    return obj

getKeysVk, getKeysTelegram :: Object -> StateT Config IO Object
getKeysVk obj =
    let response = case HM.lookup "response" obj of
            Just (Object obj) -> obj
            _                 -> HM.empty
    in return response
getKeysTelegram

askRequest :: Config -> IO (Response Value)
askRequest = undefined

sendRequest :: StateT Config IO ()
sendRequest = undefined

unpackUpdates :: Value -> Object
unpackUpdates = undefined