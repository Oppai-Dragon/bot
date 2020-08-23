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

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import qualified Control.Monad.Trans.Reader as MonadR
import qualified Control.Monad.Trans.State.Strict as MonadSS

import qualified Network.HTTP.Client as HTTPClient

type Config = A.Object

type App = MonadSS.StateT Config.Handle IO

type BotApp = MonadR.ReaderT Bot App

type ObjApp = MonadR.ReaderT A.Object App

type ReqApp = MonadSS.StateT HTTPClient.Request App

data Handle =
  Handle
    { hConfig :: Config
    , hLog :: Log.Handle
    }
  deriving (Show, Eq)

new :: IO Config.Handle
new = do
  conf <- setConfig
  logHandle <- Log.new
  return $ Config.Handle conf logHandle

modifyConfig :: (Config -> Config) -> App ()
modifyConfig func = do
  (Config.Handle config logHandle) <- getApp
  let newConfig = func config
  putApp (Config.Handle newConfig logHandle)

testHandle :: Config.Handle
testHandle = Config.Handle testConfig (Log.Handle "" Nothing)

testConfig :: A.Object
testConfig =
  HM.fromList
    [ ("bot", A.String "Vk")
    , ( "start_request"
      , (A.Object . HM.fromList)
          [ ( "path"
            , A.String "https://api.vk.com/method/groups.getLongPollServer")
          , ("params", (A.Array . V.fromList) ["group_id", "access_token", "v"])
          , ("got", A.String "response")
          ])
    , ("random_id", A.Number 0)
    , ("message", A.String "")
    , ("msgField", A.String "message")
    , ("attachment", A.String "")
    , ( "access_token"
      , A.String
          "d044ae47cea77daa95aa6e5a49fe7fbe5c5deb1fdf54eaf13b23c8d5f88fb277de05702378262ef53d1b3")
    , ( "keyboard"
      , (A.Object . HM.fromList)
          [ ( "keyboard"
            , A.String
                "{\"one_time\":true,\"buttons\":[[{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"1\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"2\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"3\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"4\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"5\"},\"color\":\"primary\"}]]}")
          ])
    , ("peer_id", A.Number 200000000099)
    , ("group_id", A.Number 152071194)
    , ("v", A.String "5.103")
    , ("repeatN", A.Number 1)
    , ( "repeatMsg"
      , A.String
          "At the moment, I repeat what you said times. Press the button with the number, with the desired number of repetitions.")
    , ( "helpMsg"
      , A.String
          "Hey. I am a simple echo-bot - I write back what they wrote to me. If you want to change how many times I reply to one of your messages, then write /repeat ")
    , ("logLevel", A.String "debug")
    ]
