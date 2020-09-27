module Config
  ( Config
  , App
  , ObjApp
  , ReqApp
  , Config.Handle(..)
  , Config.new
  , setBot
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
import qualified Data.Aeson.Types as AT
import qualified Network.HTTP.Client as HTTPClient
import System.IO.Unsafe (unsafePerformIO)

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

new :: IO Config.Handle
new = do
  Config.Handle <$> setConfig <*> Log.new <*> return Vk >>= setBot

setBot :: Config.Handle -> IO Config.Handle
setBot configHandle@(Config.Handle {hConfig = config, hLog = logHandle}) = do
  let maybeBot = AT.parseMaybe (\x -> x A..: "bot" >>= A.parseJSON) config
  case maybeBot of
    Just bot ->
      infoM logHandle "Bot is readable" >> return configHandle {hBot = bot}
    Nothing ->
      errorM
        logHandle
        "Can't read bot name, check his name in Config.json with name in Bot.hs" >>
      infoM logHandle "Will use Vk implementation" >>
      return configHandle

modifyConfig :: (Config -> Config) -> App ()
modifyConfig func = do
  configHandle <- getApp
  let newConfig = func $ hConfig configHandle
  putApp configHandle {hConfig = newConfig}

testHandle :: Config.Handle
{-# NOINLINE testHandle #-}
testHandle = unsafePerformIO Config.new

testConfig :: A.Object
{-# NOINLINE testConfig #-}
testConfig = unsafePerformIO setConfig
