module Request.Exception
  ( tryHttpJson
  , tryParseRequest
  , handleJsonResponse
  , tryUnpackResponseHttpJson
  ) where

import Base
import Config
import Log

import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import GHC.Stack
import qualified Network.HTTP.Simple as HTTPSimple

tryHttpJson ::
     HasCallStack => IO (HTTPSimple.Response A.Value) -> MaybeT App A.Value
tryHttpJson ioResponse = do
  Config.Handle {hLog = logHandle} <- liftApp getApp
  responseEither <- liftIO $ try ioResponse
  case responseEither of
    Right response -> return $ HTTPSimple.getResponseBody response
    Left err -> do
      liftIO . logError logHandle $ show err
      MaybeT $ return Nothing

tryParseRequest ::
     HasCallStack
  => IO HTTPSimple.Request
  -> Log.Handle
  -> MaybeT IO HTTPSimple.Request
tryParseRequest ioReq logHandle = do
  reqEither <- liftIO $ try ioReq
  case reqEither of
    Right req -> return req
    Left err -> do
      liftIO . logError logHandle $ show err
      MaybeT $ return Nothing

handleJsonResponse :: HasCallStack => A.Value -> MaybeT App A.Object
handleJsonResponse value = do
  Config.Handle {hLog = logHandle} <- liftApp getApp
  let obj = fromObject value
  let requestFailedObj = fromObject $ getValue ["error"] obj
  let errorDescr = T.unpack . fromString $ getValue ["description"] obj
  let errorNum = Base.toInteger $ getValue ["error_code"] obj
  let failCase = MaybeT $ return Nothing
  let succuseCase = return obj
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
               A.Bool True -> succuseCase
               _ -> succuseCase

tryUnpackResponseHttpJson ::
     HasCallStack => IO (HTTPSimple.Response A.Value) -> MaybeT App A.Value
tryUnpackResponseHttpJson responseJson =
  getValue ["response"] <$> (tryHttpJson responseJson >>= handleJsonResponse)
