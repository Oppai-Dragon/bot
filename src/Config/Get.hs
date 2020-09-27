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
  ) where

import Base
import Config
import Log

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HTTPClient

type Field = T.Text

type Method = T.Text

getStartRequest, getSendRequest, getAskRequest ::
     Config.Handle -> IO HTTPClient.Request
getStartRequest = getRequest "start_request"

getAskRequest = getRequest "ask_request"

getSendRequest = getRequest "send_request"

getRequest :: Field -> Config.Handle -> IO HTTPClient.Request
getRequest nameReq Config.Handle { hConfig = config
                                 , hLog = logHandle
                                 , hBot = bot
                                 } =
  let requestObj = getRequestObj nameReq config
      requestPath = getRequestPath requestObj
      requestParams = getRequestParams requestObj
      requestObjBool = HM.null requestObj
      requestPathBool = T.null requestPath
      requestParamsBool = null requestParams
      isRequestCollected =
        all not [requestObjBool, requestPathBool, requestParamsBool]
   in if isRequestCollected
        then return $ buildRequest requestPath requestParams config
        else do
          errorM
            logHandle
            "Request isn't collected, will be used default request from http-client"
          when requestObjBool . warningM logHandle $
            "Can't find " <>
            T.unpack nameReq <> " object in " <> show bot <> ".json"
          when requestPathBool . warningM logHandle $
            "Can't find field \"path\" in { \"" <>
            T.unpack nameReq <> "\": ...} in " <> show bot <> ".json"
          when requestParamsBool . warningM logHandle $
            "Can't find field \"params\" in { \"" <>
            T.unpack nameReq <> "\": ...} in " <> show bot <> ".json"
          return HTTPClient.defaultRequest

getApiRequest :: Method -> Config -> HTTPClient.Request
getApiRequest apiMethod conf = undefined

getRequestObj :: Field -> Config -> A.Object
getRequestObj field = fromObject . getValue [field]

getRequestPath :: A.Object -> Field
getRequestPath = fromString . getValue ["path"]

getRequestParams :: A.Object -> [(Field, Field)]
getRequestParams obj =
  let value = getValue ["params"] obj
   in if isArray value
        then map (\x -> (x, x)) $ fromArrString value
        else []

buildRequest :: Field -> [(Field, Field)] -> Config -> HTTPClient.Request
buildRequest path params conf =
  let initRequest =
        HTTPClient.parseRequest_ . T.unpack $ parseRequestPath path conf
      findAndConvert field =
        case getValue [field] conf of
          A.String text -> TE.encodeUtf8 text
          A.Number num ->
            TE.encodeUtf8 . T.pack . show . valueToInteger $ A.Number num
          A.Bool bool -> TE.encodeUtf8 . T.pack $ show bool
          _ -> ""
      pairs = map (\(l, r) -> (TE.encodeUtf8 l, findAndConvert r)) params
      request =
        (HTTPClient.urlEncodedBody pairs initRequest)
          {HTTPClient.method = "POST"}
   in request

buildApiRequest

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
