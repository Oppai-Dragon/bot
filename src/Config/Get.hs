module Config.Get
  ( getStartRequest
  , getAskRequest
  , getSendRequest
  , getPhotosGetMsgUploadServerReq
  , getDocsGetMsgUploadServerReq
  , getPhotosSaveMsgPhotoReq
  , getDocsSaveReq
  , getPhotosGetReq
  , getRequest
  , getApiRequest
  , getRequestObj
  , getRequestPath
  , getRequestParams
  , buildRequest
  , mapFindAndConvert
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
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Simple as HTTPSimple

type Field = T.Text

type Method = T.Text

getStartRequest, getSendRequest, getAskRequest, getPhotosGetMsgUploadServerReq, getDocsGetMsgUploadServerReq, getPhotosSaveMsgPhotoReq, getDocsSaveReq, getPhotosGetReq ::
     Config.Handle -> IO HTTPClient.Request
getStartRequest = getRequest "start_request"

getAskRequest = getRequest "ask_request"

getSendRequest = getRequest "send_request"

getPhotosGetMsgUploadServerReq = getApiRequest "photos.getMessagesUploadServer"

getDocsGetMsgUploadServerReq = getApiRequest "docs.getMessagesUploadServer"

getPhotosSaveMsgPhotoReq = getApiRequest "photos.saveMessagesPhoto"

getDocsSaveReq = getApiRequest "docs.save"

getPhotosGetReq = getApiRequest "photos.get"

getRequest :: Field -> Config.Handle -> IO HTTPClient.Request
getRequest nameReq Config.Handle { hConfig = config
                                 , hLog = logHandle
                                 , hBot = bot
                                 } =
  let requestObj = getRequestObj nameReq config
      requestPath = getRequestPath requestObj
      requestQuery = getRequestQuery requestObj
      requestParams = getRequestParams requestObj
      requestObjBool = HM.null requestObj
      requestPathBool = T.null requestPath
      requestParamsAndQueryBool = null requestParams && null requestQuery
      isRequestCollected =
        all not [requestObjBool, requestPathBool, requestParamsAndQueryBool]
   in if isRequestCollected
        then return $ buildRequest requestPath requestQuery requestParams config
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
          when requestParamsAndQueryBool . warningM logHandle $
            "Can't find field \"params\" in { \"" <>
            T.unpack nameReq <> "\": ...} in " <> show bot <> ".json"
          return HTTPClient.defaultRequest

getApiRequest :: Method -> Config.Handle -> IO HTTPClient.Request
getApiRequest apiMethod Config.Handle { hConfig = config
                                      , hLog = logHandle
                                      , hBot = bot
                                      } =
  let apiRequestObj = getRequestObj "api_request" config
      apiMethodObj = fromObject $ getValue ["api_methods", apiMethod] config
      methodParamArr = getValue ["params"] apiMethodObj
      requestQuery =
        getRequestQuery . fromObject $ getValue ["query"] apiMethodObj
      requestObj = insertWithPush "params" methodParamArr apiRequestObj
      requestPath = getRequestPath apiRequestObj <> apiMethod
      requestParams = getRequestParams requestObj
      requestObjBool = HM.null requestObj
      requestPathBool = T.null requestPath
      requestParamsAndQueryBool = null requestParams && null requestQuery
      isRequestCollected =
        all not [requestObjBool, requestPathBool, requestParamsAndQueryBool]
   in if isRequestCollected
        then return $ buildRequest requestPath requestQuery requestParams config
        else do
          errorM
            logHandle
            "Request isn't collected, will be used default request from http-client"
          when requestObjBool . warningM logHandle $
            "Can't find " <>
            T.unpack apiMethod <> " object in " <> show bot <> ".json"
          when requestPathBool . warningM logHandle $
            "Can't find field \"path\" in { \"" <>
            T.unpack apiMethod <> "\": ...} in " <> show bot <> ".json"
          when requestParamsAndQueryBool . warningM logHandle $
            "Can't find field \"params\" in { \"" <>
            T.unpack apiMethod <> "\": ...} in " <> show bot <> ".json"
          return HTTPClient.defaultRequest

getRequestObj :: Field -> Config -> A.Object
getRequestObj field = fromObject . getValue [field]

getRequestPath :: A.Object -> Field
getRequestPath = fromString . getValue ["path"]

getRequestQuery :: A.Object -> HTTPSimple.Query
getRequestQuery =
  map (\(field, value) -> (TE.encodeUtf8 field, Just $ toBS value)) . HM.toList

getRequestParams :: A.Object -> [(Field, Field)]
getRequestParams obj =
  let value = getValue ["params"] obj
   in if isArray value
        then map (\x -> (x, x)) $ fromArrString value
        else []

buildRequest ::
     Field
  -> HTTPSimple.Query
  -> [(Field, Field)]
  -> Config
  -> HTTPClient.Request
buildRequest path query params conf =
  let initRequest =
        HTTPClient.parseRequest_ . T.unpack $ parseRequestPath path conf
      pairs = mapFindAndConvert conf params
      request =
        HTTPClient.setQueryString
          query
          (HTTPClient.urlEncodedBody pairs initRequest)
            {HTTPClient.method = "POST"}
   in request

mapFindAndConvert ::
     Config -> [(Field, Field)] -> [(BS.ByteString, BS.ByteString)]
mapFindAndConvert _ [] = []
mapFindAndConvert conf ((l, r):rest) =
  let value = getValue [r] conf
   in case value of
        A.String _ ->
          (TE.encodeUtf8 l, toBS value) : mapFindAndConvert conf rest
        A.Number _ ->
          (TE.encodeUtf8 l, toBS value) : mapFindAndConvert conf rest
        A.Bool _ -> (TE.encodeUtf8 l, toBS value) : mapFindAndConvert conf rest
        A.Object obj ->
          (map (\(field, valueX) -> (TE.encodeUtf8 field, toBS valueX)) .
           HM.toList)
            obj <>
          mapFindAndConvert conf rest
        _ -> mapFindAndConvert conf rest

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
