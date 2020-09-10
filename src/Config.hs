module Config
  ( Config
  , App
  , BotApp
  , ObjApp
  , ReqApp
  , Config.Handle(..)
  , Config.new
  , modifyConfig
  , testHandle
  , testConfig
  ) where

import Base
import Bot
import Config.Set
import Log

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified Data.Aeson as A
import qualified Network.HTTP.Client as HTTPClient
import System.IO.Unsafe (unsafePerformIO)

type Config = A.Object

type App = StateT Config.Handle IO

type BotApp = ReaderT Bot App

type ObjApp = ReaderT A.Object App

type ReqApp = StateT HTTPClient.Request App

data Handle =
  Handle
    { hConfig :: Config
    , hLog :: Log.Handle
    }
  deriving (Show, Eq)

new :: IO Config.Handle
new = do
  conf <- setConfig
  Config.Handle conf <$> Log.new

modifyConfig :: (Config -> Config) -> App ()
modifyConfig func = do
  (Config.Handle config logHandle) <- getApp
  let newConfig = func config
  putApp (Config.Handle newConfig logHandle)

testHandle :: Config.Handle
{-# NOINLINE testHandle #-}
testHandle = unsafePerformIO Config.new

testConfig :: A.Object
{-# NOINLINE testConfig #-}
testConfig = unsafePerformIO setConfig
