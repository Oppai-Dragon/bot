module Config.Get
  ( getStartRequest
  , getAskRequest
  , getSendRequest
  , getRequest
  , getRequestObj
  , getRequestPath
  , getRequestParams
  , buildRequest
  , valueToInteger
  , parseRequestPath
  , getUnpackField
  , getKeyboard
  , getRepeatMsg
  , getBot
  ) where

import Base
import Bot
import Config
import Log

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import qualified Network.HTTP.Client as HTTPClient

type Field = T.Text

type Fields = [Field]

getStartRequest, getSendRequest, getAskRequest :: Config -> HTTPClient.Request
getStartRequest = getRequest "start_request"

getAskRequest = getRequest "ask_request"

getSendRequest = getRequest "send_request"

getRequest :: Field -> Config -> HTTPClient.Request
getRequest nameReq conf =
  let requestObj = getRequestObj nameReq conf
      requestPath = getRequestPath requestObj
      requestParams = getRequestParams requestObj
      isRequestCollected =
        all not [HM.null requestObj, T.null requestPath, null requestParams]
   in if isRequestCollected
        then buildRequest requestPath requestParams conf
        else HTTPClient.defaultRequest

getRequestObj :: Field -> Config -> A.Object
getRequestObj field conf =
  case getValue [field] conf of
    A.Object obj -> obj
    _ -> HM.empty

getRequestPath :: A.Object -> Field
getRequestPath obj =
  case getValue ["path"] obj of
    A.String path -> path
    _ -> ""

getRequestParams :: A.Object -> Fields
getRequestParams obj =
  case getValue ["params"] obj of
    A.Array vector -> map (\(A.String x) -> x) $ V.toList vector
    _ -> []

buildRequest :: Field -> Fields -> Config -> HTTPClient.Request
buildRequest path params conf =
  let initRequest = HTTPClient.parseRequest_ . T.unpack $ parseRequestPath path conf
      toBS field =
        case getValue [field] conf of
          A.String text -> TE.encodeUtf8 text
          A.Number num ->
            TE.encodeUtf8 . T.pack . show . valueToInteger $ A.Number num
          A.Bool bool -> TE.encodeUtf8 . T.pack . show $ bool
          _ -> ""
      pairs = map (\x -> (TE.encodeUtf8 x, toBS x)) params
      request = (HTTPClient.urlEncodedBody pairs initRequest) {HTTPClient.method = "POST"}
   in request

valueToInteger :: A.Value -> Integer
valueToInteger = Maybe.fromMaybe 0 . AT.parseMaybe A.parseJSON

parseRequestPath :: Field -> Config -> Field
parseRequestPath path conf =
  let beforeParam = T.takeWhile (/= '<') path
      param = T.tail . T.takeWhile (/= '>') . T.dropWhile (/= '<') $ path
      afterParam = T.tail $ T.dropWhile (/= '>') path
      paramValue =
        case getValue [param] conf of
          A.String text -> text
          _ -> ""
   in case T.find (== '>') path of
        Just _ -> beforeParam <> paramValue <> afterParam
        Nothing -> path

getUnpackField :: Field -> Config -> Field
getUnpackField field conf =
  case getValue [field, "got"] conf of
    A.String text -> text
    _ -> ""

getKeyboard :: Config -> [(T.Text, A.Value)]
getKeyboard conf =
  case getValue ["keyboard"] conf of
    A.Object obj -> HM.toList obj
    _ -> []

getRepeatMsg :: Config -> A.Value
getRepeatMsg conf =
  let A.Number repeatN = getValue ["repeatN"] conf
      repeatNText = T.pack . takeWhile (/= '.') $ show repeatN
      A.String repeatMsg = getValue ["repeatMsg"] conf
   in A.String $
      T.unwords $
      map
        (\x ->
           if x == "said"
             then "said " <> repeatNText
             else x) $
      T.words repeatMsg

getBot :: Config.Handle -> IO Bot
getBot (Config.Handle conf logHandle) = do
  let botT = case getValue ["bot"] conf of
        A.String name -> name
        _ -> ""
  let errorLog = const $ errorM logHandle "Can't read bot name, check his name in Config.json with name in Bot.hs"
  let infoLog = const $ infoM logHandle "Bot is readable"
  let bot = read $ T.unpack botT :: Bot
  result <- tryM $ return bot
  either infoLog errorLog result
  return bot
