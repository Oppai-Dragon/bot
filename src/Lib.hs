module Lib
  ( runBot
  ) where

import Config
import Config.Get
import Helpers
import Log.Methods

import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HM

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import Network.HTTP.Simple

runBot :: StateT Config IO ()
runBot = do
  config <- get
  let bot = getBot config
  lift $ infoM " " $ show bot <> " bot is selected."
  let request = getStartRequest config
  lift $ debugM "Lib:27" "Start Request"
  response <- lift $ httpJSON request
  lift $ debugM " " "Success"
  let json = getResponseBody response
  localConfig <- runReaderT (getKeys json) bot
  modify $ HM.union localConfig
  liveSession

liveSession :: StateT Config IO ()
liveSession = do
  config <- get
  let request = getAskRequest config
  lift $ debugM "Lib:39" "Ask Request"
  response <- lift $ httpJSON request
  lift $ debugM " " "Success"
  let json = getResponseBody response
  let bot = getBot config
  updates' <- unpackUpdates json
  updates <- checkUpdates updates'
  if HM.null updates
    then liveSession
    else lift (debugM "Lib:44" "The message is received.") >>
         runReaderT updateConfig ((updates, json), bot) >>
         echoMessage

echoMessage :: StateT Config IO ()
echoMessage = do
  config <- get
  let repeatN =
        case getValue ["repeatN"] config of
          Number n -> valueToInteger $ Number n
          _ -> 1
  lift $ debugM "Lig:55" $ "Number of repetitions " <> show repeatN <> "."
  sendMessage repeatN

sendMessage :: Integer -> StateT Config IO ()
sendMessage 0 = liveSession
sendMessage n = do
  updateRandomId
  config <- get
  let request = getSendRequest config
  let modifyReq req =
        case getValue ["lastMsg"] config of
          String "/repeat" -> addingKeyboard req config
          _ -> req
  lift $ debugM "Lib:70" "Send Request"
  _ <- lift $ httpBS $ modifyReq request
  lift $ debugM " " "Success"
  sendMessage (n - 1)
