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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTPClient

type Field = T.Text

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
getRequestObj field = fromObj . getValue [field]

getRequestPath :: A.Object -> Field
getRequestPath = fromString . getValue ["path"]

getRequestParams :: A.Object -> [[(Field, Field)]]
getRequestParams obj =
  let valueArr@(A.Array vector) = getValue ["params"] obj
   in case V.toList vector of
        A.String _:_ ->
          maybe [] (map (\x -> [(x, x)])) $ AT.parseMaybe A.parseJSON valueArr
        A.Object _:_ ->
          maybe [] (map (map (\(l, A.String r) -> (l, r)) . HM.toList)) $
          AT.parseMaybe A.parseJSON valueArr
        _ -> []

buildRequest :: Field -> [[(Field, Field)]] -> Config -> HTTPClient.Request
buildRequest path params conf =
  let initRequest =
        HTTPClient.parseRequest_ . T.unpack $ parseRequestPath path conf
      toBS field =
        case getValue [field] conf of
          A.String text -> TE.encodeUtf8 text
          A.Number num ->
            TE.encodeUtf8 . T.pack . show . valueToInteger $ A.Number num
          A.Bool bool -> TE.encodeUtf8 . T.pack $ show bool
          _ -> ""
      pairs = map (\(l, r) -> (TE.encodeUtf8 l, toBS r)) $ concat params
      request =
        (HTTPClient.urlEncodedBody pairs initRequest)
          {HTTPClient.method = "POST"}
   in request

parseRequestPath :: Field -> Config -> Field
parseRequestPath path conf =
  let beforeParam = T.takeWhile (/= '<') path
      param = T.tail . T.takeWhile (/= '>') . T.dropWhile (/= '<') $ path
      afterParam = T.tail $ T.dropWhile (/= '>') path
      paramValue =
        case getValue [param] conf of
          A.String text -> text
          _ -> ""
      newPath = beforeParam <> paramValue <> afterParam
   in case T.find (== '>') path of
        Just _ -> parseRequestPath newPath conf
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
  let maybeBot = AT.parseMaybe (\x -> x A..: "bot" >>= A.parseJSON) conf
  case maybeBot of
    Just bot -> infoM logHandle "Bot is readable" >> return bot
    Nothing ->
      errorM
        logHandle
        "Can't read bot name, check his name in Config.json with name in Bot.hs" >>
      infoM logHandle "Will use Vk implementation" >>
      return Vk
