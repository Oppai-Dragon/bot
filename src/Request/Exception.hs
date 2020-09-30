module Request.Exception
  ( tryHttpJson
  , tryParseRequest
  , handleJsonResponse
  ) where

import Base
import Config
import Log

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import GHC.Stack
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Simple as HTTPSimple

tryHttpJson :: HasCallStack => IO (HTTPSimple.Response A.Value) -> App A.Value
tryHttpJson ioResponse = do
  Config.Handle {hLog = logHandle} <- getApp
  responseEither <- liftIO $ tryM ioResponse
  case responseEither of
    Right response -> do
      let json = HTTPSimple.getResponseBody response
      return json
    Left err -> do
      liftIO . errorM logHandle $ show err
      return A.Null

tryParseRequest ::
     HasCallStack => IO HTTPSimple.Request -> Log.Handle -> IO HTTPSimple.Request
tryParseRequest ioReq logHandle = do
  reqEither <- liftIO $ tryM ioReq
  case reqEither of
    Right req -> return req
    Left err -> do
      liftIO . errorM logHandle $ show err
      liftIO $
        warningM logHandle "Will be used Network.HTTP.Client.defaultRequest"
      return HTTPClient.defaultRequest

handleJsonResponse :: HasCallStack => A.Value -> App A.Object
handleJsonResponse value = do
  Config.Handle {hLog = logHandle} <- getApp
  let obj = fromObject value
  let failCase = return HM.empty
  if HM.null obj
    then liftIO (errorM logHandle "Json from response is empty") >> failCase
    else case HM.keys obj of
           ["error"] ->
             (liftIO . errorM logHandle $
              "Failed request: " <> (show . fromObject . getValue ["error"]) obj) >>
             failCase
           _ -> return obj
