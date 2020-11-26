module Request.Exception
  ( tryHttpJson
  , tryParseRequest
  , handleJsonResponse
  , tryUnpackResponseHttpJson
  ) where

import Base
import Config
import Log

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import GHC.Stack
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Simple as HTTPSimple

tryHttpJson ::
     HasCallStack => IO (HTTPSimple.Response A.Value) -> App (Maybe A.Value)
tryHttpJson ioResponse = do
  Config.Handle {hLog = logHandle} <- getApp
  responseEither <- liftIO $ try ioResponse
  case responseEither of
    Right response -> do
      let json = HTTPSimple.getResponseBody response
      return $ Just json
    Left err -> do
      liftIO . logError logHandle $ show err
      return Nothing

tryParseRequest ::
     HasCallStack
  => IO HTTPSimple.Request
  -> Log.Handle
  -> IO (Maybe HTTPSimple.Request)
tryParseRequest ioReq logHandle = do
  reqEither <- liftIO $ try ioReq
  case reqEither of
    Right req -> return $ Just req
    Left err -> do
      liftIO . logError logHandle $ show err
      return Nothing

handleJsonResponse :: HasCallStack => A.Value -> App (Maybe A.Object)
handleJsonResponse value = do
  Config.Handle {hLog = logHandle} <- getApp
  let obj = fromObject value
  let requestFailedObj = fromObject $ getValue ["error"] obj
  let errorDescr = T.unpack . fromString $ getValue ["description"] obj
  let errorNum = Base.toInteger $ getValue ["error_code"] obj
  let failCase = return Nothing
  if HM.null obj
    then do
      liftIO $ logError logHandle "Json from response is empty"
      failCase
    else case HM.keys obj of
           ["error"] -> do
             liftIO . logError logHandle $
               "Failed request: " <> show requestFailedObj
             failCase
           _ ->
             case getValue ["ok"] obj of
               A.Bool False -> do
                 liftIO . logError logHandle $
                   "Error " <> show errorNum <> ": " <> errorDescr
                 failCase
               A.Bool True -> return obj
               _ -> return obj

tryUnpackResponseHttpJson ::
     HasCallStack => IO (HTTPSimple.Response A.Value) -> App (Maybe A.Value)
tryUnpackResponseHttpJson =
  fmap (getValue ["response"]) . (=<<) handleJsonResponse . tryHttpJson
