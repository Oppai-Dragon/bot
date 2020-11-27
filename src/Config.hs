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

import Base
import Bot
import Config.Set
import Log

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Maybe
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

maybeNew :: IO (Maybe Config.Handle)
maybeNew = do
  maybeConfig <- maybeSetConfig
  maybeLogHandle <- Log.maybeNew
  let maybeBot =
        AT.parseMaybe (\x -> x A..: "bot" >>= A.parseJSON) $
        fromJust maybeConfig
  if and [isJust maybeConfig, isJust maybeLogHandle, isJust maybeBot]
    then return $
         Just
           Config.Handle
             { hConfig = fromJust maybeConfig
             , hLog = fromJust maybeLogHandle
             , hBot = fromJust maybeBot
             }
    else return Nothing

modifyConfig :: (Config -> Config) -> App ()
modifyConfig func = do
  configHandle <- getApp
  let newConfig = func $ hConfig configHandle
  putApp configHandle {hConfig = newConfig}

modifyReq :: (HTTPClient.Request -> HTTPClient.Request) -> ReqApp ()
modifyReq = modifyApp
