module Session
  ( runBot
  ) where

import Base
import Bot
import qualified Bot.Telegram as Telegram
import Config
import Config.Update
import Log
import Request

import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

type Updates = A.Object

runBot :: App ()
runBot = do
  Config.Handle {hLog = logHandle, hBot = bot} <- getApp
  liftIO $ logInfo logHandle $ show bot <> " bot is selected."
  maybeObj <- runMaybeT maybeTStartRequest
  case maybeObj of
    Just obj -> do
      localConfig <- getKeys obj
      modifyConfig $ HM.union localConfig
      liveSession
      liftIO $ logFinishMsg logHandle
    Nothing -> return ()

liveSession :: App ()
liveSession = do
  maybeObj <- runMaybeT maybeTAskRequest
  case maybeObj of
    Just obj -> getUpdates obj >>= echoMessage >> liveSession
    Nothing -> return ()

echoMessage :: [Updates] -> App ()
echoMessage (updates:rest) = do
  Config.Handle {hConfig = config, hLog = logHandle, hBot = bot} <- getApp
  updates' <-
    case bot of
      Vk -> return updates
      Telegram -> Telegram.checkUpdate updates
  if HM.null updates'
    then echoMessage rest
    else do
      updateConfig updates bot
      let repeatN = Base.toInteger $ getValue ["repeatN"] config
      liftIO . logDebug logHandle $
        "Number of repetitions " <> show repeatN <> "."
      sendMessage repeatN
      echoMessage rest
echoMessage _ = return ()

sendMessage :: Integer -> App ()
sendMessage 0 = return ()
sendMessage n = do
  updateRandomId
  _ <- runMaybeT maybeTSendRequest
  sendMessage (n - 1)
