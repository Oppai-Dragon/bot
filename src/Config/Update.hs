module Config.Update
  ( getKeys
  , getLastObj
  , updateConfig
  , updateRepeatN
  , parseMessage
  , msgHandler
  , updateRandomId
  , getUpdates
  ) where

import Base
import Bot
import qualified Bot.Telegram as Telegram
import qualified Bot.Vk as Vk
import Config
import Config.Get
import Log

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Text.Parsec as P

type Message = T.Text

type Updates = A.Object

getKeys :: A.Object -> App Updates
getKeys obj = do
  Config.Handle {hConfig = config, hConfigLogHandle = logHandle, hConfigBot = bot} <- getApp
  let field = getUnpackField "start_request" config
  let updateObj =
        case AT.parseMaybe (A..: field) obj of
          Just (A.Array vector) -> getLastObj (A.Array vector)
          Just (A.Object responseObj) -> responseObj
          _ -> HM.empty
  if HM.null updateObj
    then do
      liftIO $ logWarning logHandle "Can't unpack response of request"
      return HM.empty
    else updateObj &
         case bot of
           Bot.Vk -> Vk.getKeys
           Bot.Telegram -> Telegram.getKeys

getLastObj :: A.Value -> Updates
getLastObj value =
  let arrayObj = HM.singleton "array" value
   in case AT.parseMaybe (A..: "array") arrayObj :: Maybe [Updates] of
        Just objArr ->
          if null objArr
            then HM.empty
            else last objArr
        Nothing -> HM.empty

updateConfig :: Updates -> Bot -> App ()
updateConfig updates bot = do
  msg <-
    (=<<) parseMessage $
    updates &
    case bot of
      Bot.Vk -> runSubApp Vk.update
      Bot.Telegram -> runSubApp Telegram.update
  updateRepeatN msg
  msgHandler msg

updateRepeatN :: Message -> App ()
updateRepeatN msg = do
  Config.Handle {hConfig = config} <- getApp
  let lastMsg = getValue ["lastMsg"] config
  let msgStr = T.unpack msg
  let oldRepeatN = getValue ["repeatN"] config
  let repeatN =
        case lastMsg of
          A.String "/repeat" -> A.Number . fromMaybe 1 $ readMaybe msgStr
          _ -> oldRepeatN
  modifyConfig $ HM.insert "repeatN" repeatN

parseMessage :: Message -> App Message
parseMessage msg = do
  Config.Handle {hConfigLogHandle = logHandle} <- getApp
  let parseFunc = do
        _ <- P.char '['
        name1 <- P.many $ P.letter P.<|> P.digit
        _ <- P.char '|'
        _ <- P.string $ "@" <> name1
        _ <- P.char ']'
        _ <- P.space
        str <- P.many1 P.anyChar
        return str :: P.Parsec String String String
  case P.runParser (P.try parseFunc P.<|> P.many P.anyChar) "" "" (T.unpack msg) of
    Left err -> do
      liftIO . logWarning logHandle $ show err
      return ""
    Right x -> return $ T.pack x

msgHandler :: Message -> App ()
msgHandler msg = do
  Config.Handle {hConfig = config} <- getApp
  let msgField =
        case getValue ["msgField"] config of
          A.String x -> x
          _ -> ""
  let msgResult =
        case msg of
          "/help" -> getValue ["helpMsg"] config
          "/repeat" -> getRepeatMsg config
          _ -> A.String msg
  let localConfig =
        HM.fromList [("lastMsg", A.String msg), (msgField, msgResult)]
  modifyConfig $ HM.union localConfig

updateRandomId :: App ()
updateRandomId = do
  randomN <- liftIO getRandomInteger
  let randomId = A.Number $ Scientific.scientific randomN 0
  modifyConfig $ HM.insert "random_id" randomId

getUpdates :: A.Object -> App [Updates]
getUpdates json = do
  Config.Handle {hConfig = config, hConfigBot = bot} <- getApp
  let field = getUnpackField "ask_request" config
  let updates = fromArrObject $ getValue [field] json
  case bot of
    Vk -> do
      Vk.updateKeys json
      return updates
    Telegram -> return updates
