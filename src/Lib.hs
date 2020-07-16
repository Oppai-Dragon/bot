module Lib
    ( runBot
    ) where

import Bot
import Config
import Config.Get
import Helpers

import           Data.Aeson
import qualified Data.HashMap.Strict    as HM

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
    let bot = getBot config
    localConfig <- runReaderT (getKeys json) bot
    put $ HM.union localConfig config
    liveSession

liveSession :: StateT Config IO ()
liveSession = do
    config <- get
    let request = getAskRequest config
    response <- lift $ httpJSON request
    let json = getResponseBody response
    let bot = getBot config
    updates <- unpackUpdates json
    runReaderT updateConfig ((updates,json),bot)
    bool <- runReaderT isMsgNotNeed (updates,bot)
    if bool
        then liveSession
        else echoMessage
    liveSession

echoMessage :: StateT Config IO ()
echoMessage = do
    config <- get
    let repeatN = case getValue ["repeatN"] config of
            Number n -> valueToInteger $ Number n
            _        -> 1
    sendMessage repeatN

sendMessage :: Integer -> StateT Config IO ()
sendMessage 0 = return ()
sendMessage n = do
    updateRandomId
    config <- get
    let request = getSendRequest config
    let modifyReq req = case getValue ["lastMsg"] config of
            String "/repeat" -> addingKeyboard req config
            _                -> req
    response <- lift $ httpBS $ modifyReq request
    sendMessage (n-1)