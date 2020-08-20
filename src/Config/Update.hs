module Config.Update
  ( getKeys
  , getLastObj
  , checkUpdates
  , updateConfig
  , updateRepeatN
  , msgHandler
  , updateRandomId
  , unpackUpdates
  ) where

import Base
import Bot
import qualified Bot.Telegram as Telegram
import qualified Bot.Vk as Vk
import Config
import Config.Get

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Char as C
import qualified Data.Function as Func

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type Message = T.Text

type Updates = A.Object

getKeys :: A.Value -> BotApp Updates
getKeys (A.Object obj) = do
  bot <- askApp
  (Config.Handle config _) <- fromApp getApp
  let field = getUnpackField "start_request" config
  let updateObj =
        case AT.parseMaybe (A..: field) obj of
          Just (A.Array vector) -> getLastObj (A.Array vector)
          Just (A.Object responseObj) -> responseObj
          _ -> HM.empty
  updateObj Func.&
    case bot of
      Bot.Vk -> Vk.getKeys
      Bot.Telegram -> Telegram.getKeys
getKeys _ = pure HM.empty

getLastObj :: A.Value -> Updates
getLastObj value =
  let arrayObj = HM.singleton "array" value
   in case AT.parseMaybe (A..: "array") arrayObj :: Maybe [Updates] of
        Just objArr ->
          if null objArr
            then HM.empty
            else last objArr
        Nothing -> HM.empty

checkUpdates :: Updates -> App Updates
checkUpdates updates = do
  (Config.Handle config _) <- getApp
  let updateIdOld = getValue ["offset"] config
  case HM.lookup "update_id" updates of
    Just value ->
      if updateIdOld == value
        then return HM.empty
        else return updates
    Nothing -> return updates

updateConfig :: Updates -> A.Value -> BotApp ()
updateConfig updates (A.Object response) = do
  bot <- askApp
  msg <-
    fromApp $
    updates Func.&
    case bot of
      Bot.Vk -> runRApp (Vk.update response)
      Bot.Telegram -> runRApp Telegram.update
  fromApp $ updateRepeatN msg
  fromApp $ msgHandler msg
updateConfig _ _ = return ()

updateRepeatN :: Message -> App ()
updateRepeatN msg = do
  (Config.Handle config _) <- getApp
  let lastMsg = getValue ["lastMsg"] config
  let msgStr = T.unpack msg
  let greatThenNull n =
        if n <= 0
          then 1
          else n
  let oldRepeatN = getValue ["repeatN"] config
  let repeatN =
        case lastMsg of
          A.String "/repeat" ->
            A.Number $
            if all C.isDigit msgStr
              then greatThenNull $ read msgStr
              else 1
          _ -> oldRepeatN
  modifyConfig $ HM.insert "repeatN" repeatN

msgHandler :: Message -> App ()
msgHandler msg = do
  (Config.Handle config _) <- getApp
  let msgField =
        case getValue ["msgField"] config of
          A.String x -> x
          _ -> ""
  let msgObj =
        HM.singleton msgField $
        case msg of
          "/help" -> getValue ["helpMsg"] config
          "/repeat" -> getRepeatMsg config
          _ -> A.String msg
  let localConfig = HM.singleton "lastMsg" (A.String msg) `HM.union` msgObj
  modifyConfig $ HM.union localConfig

updateRandomId :: App ()
updateRandomId = do
  randomN <- fromIO getRandomInteger
  let randomId = A.Number . read $ show randomN
  modifyConfig $ HM.insert "random_id" randomId

unpackUpdates :: A.Value -> App Updates
unpackUpdates (A.Object obj) = do
  (Config.Handle config _) <- getApp
  let field = getUnpackField "ask_request" config
  let updateObj =
        case AT.parseMaybe (A..: field) obj of
          Just (A.Array vector) -> getLastObj (A.Array vector)
          Just (A.Object responseObj) -> responseObj
          _ -> HM.empty
  return updateObj
unpackUpdates _ = return HM.empty
