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
    , getValue
    , getRandom
    ) where

import Bot
import Config

import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.HashMap.Strict    as HM
import qualified Data.Vector            as V
import qualified Data.List              as L
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import Network.HTTP.Client

import System.Random
    ( getStdRandom
    , randomR
    )

type Field = T.Text
type Fields = [Field]

getStartRequest, getSendRequest, getAskRequest :: Config -> Request
getStartRequest = getRequest "start_request"
getAskRequest = getRequest "ask_request"
getSendRequest = getRequest "send_request"

getRequest :: Field -> Config -> Request
getRequest nameReq conf =
    let
        requestObj = getRequestObj nameReq conf
        requestPath = getRequestPath requestObj
        requestParams = getRequestParams requestObj
        isRequestCollected = all not
            [HM.null requestObj
            ,T.null requestPath
            ,null requestParams]
    in if isRequestCollected
        then buildRequest requestPath requestParams conf
        else defaultRequest

getRequestObj :: Field -> Config -> Object
getRequestObj field conf =
    case getValue [field] conf of
        Object obj -> obj
        _          -> HM.empty

getRequestPath :: Object -> Field
getRequestPath obj =
    case getValue ["path"] obj of
        String path -> path
        _           -> ""

getRequestParams :: Object -> Fields
getRequestParams obj =
    case getValue ["params"] obj of
        Array vector -> map (\(String x) -> x) $ V.toList vector
        _            -> []

buildRequest :: Field -> Fields -> Config -> Request
buildRequest path params conf =
    let
        initRequest = parseRequest_ $ T.unpack $ parseRequestPath path conf
        toBS field = case getValue [field] conf of
            String text -> TE.encodeUtf8 text
            Number num  -> TE.encodeUtf8 . T.pack . show . valueToInteger $ Number num
            Bool bool   -> TE.encodeUtf8 . T.pack . show $ bool
            _           -> ""
        pairs = map (\x -> (TE.encodeUtf8 x, toBS x)) params
        request = (urlEncodedBody pairs initRequest) {method = "POST"}
    in request

valueToInteger :: Value -> Integer
valueToInteger (Number num) =
    case parseMaybe (.: "id") (HM.fromList [("id",Number num)]) of
        Just n  -> n
        Nothing -> 0

parseRequestPath :: Field -> Config -> Field
parseRequestPath path conf =
    let
        beforeParam = T.takeWhile (/='<') path
        param = T.tail . T.takeWhile (/='>') . T.dropWhile (/='<') $ path
        afterParam = T.tail $ T.dropWhile (/='>') path
        paramValue = case parseMaybe (.: param) conf of
            Just (String text) -> text
            _                  -> ""
    in case T.find (=='>') path of
        Just ch -> beforeParam <> paramValue <> afterParam
        Nothing -> path

getUnpackField :: Field -> Config -> Field
getUnpackField field conf =
    case getValue [field,"got"] conf of
        (String text) -> text
        Null          -> ""

getKeyboard :: Config -> [(T.Text,Value)]
getKeyboard conf =
    case parseMaybe (.: "keyboard") conf of
        Just (Object obj) -> HM.toList obj
        _                 -> []

getRepeatMsg :: Config -> Value
getRepeatMsg conf =
    let
        Number repeatN = getValue ["repeatN"] conf
        repeatNText = T.pack . takeWhile (/='.') $ show repeatN
        String repeatMsg = getValue ["repeatMsg"] conf
    in String $ T.unwords $
    map (\x -> if x == "said" then "said " <> repeatNText else x)
    $ T.words repeatMsg

getBot :: Config -> Bot
getBot conf =
    let (String text) =  getValue ["bot"] conf
    in read . T.unpack $ text

getValue :: Fields -> Object -> Value
getValue []           obj    = Object obj
getValue (field:rest) objOld =
    case parseMaybe (.: field) objOld of
        Just (Object objNew) -> getValue rest objNew
        Just value           -> value
        Nothing              -> Null

getRandom :: IO Integer
getRandom = getStdRandom (randomR (1,100000000000))