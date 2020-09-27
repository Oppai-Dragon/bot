module Request.Exception
  ( tryHttpJson
  ) where

import Base
import Config
import Log

import qualified Data.Aeson as A
import GHC.Stack
import qualified Network.HTTP.Simple as HTTPSimple

tryHttpJson :: HasCallStack => IO (HTTPSimple.Response A.Value) -> App A.Value
tryHttpJson responseM = do
  Config.Handle {hLog = logHandle} <- getApp
  responseEither <- liftIO $ tryM responseM
  case responseEither of
    Right response -> do
      let json = HTTPSimple.getResponseBody response
      return json
    Left err -> do
      liftIO . errorM logHandle $ show err
      return A.Null
