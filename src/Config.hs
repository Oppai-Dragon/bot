module Config
  ( Config
  , App
  , ObjApp
  , ReqApp
  , Config.Handle(..)
  , Config.maybeNew
  , modifyConfig
  , modifyReq
  ) where

import Base (getApp, modifyApp, putApp)
import Bot
import Config.Create
import Log

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Network.HTTP.Client as HTTPClient

type Config = A.Object

type App = StateT Config.Handle IO

type ObjApp = ReaderT A.Object App

type ReqApp = StateT HTTPClient.Request App

data Handle =
  Handle
    { hConfig :: Config
    , hLog :: Log.Handle
    , hBot :: Bot
    }
  deriving (Show, Eq)

maybeNew :: MaybeT IO Config.Handle
maybeNew = do
  config <- maybeTCreateConfig
  logHandle <- Log.maybeNew
  bot <-
    MaybeT . return $ AT.parseMaybe (\x -> x A..: "bot" >>= A.parseJSON) config
  return Config.Handle {hConfig = config, hLog = logHandle, hBot = bot}

modifyConfig :: (Config -> Config) -> App ()
modifyConfig func = do
  configHandle <- getApp
  let newConfig = func $ hConfig configHandle
  putApp configHandle {hConfig = newConfig}

modifyReq :: (HTTPClient.Request -> HTTPClient.Request) -> ReqApp ()
modifyReq = modifyApp
