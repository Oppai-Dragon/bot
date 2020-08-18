module Config
  ( Config
  , parsePath
  , set
  , testConfig
  ) where

import Config.Set
import Log

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

data Handle =
  Handle
    { config :: Object
    , log :: Log.Handle
    }

new :: IO Handle
new = do
  conf <- setConfig
  log <- Log.new
  return $ Handle conf log

testConfig :: Object
testConfig =
  HM.fromList
    [ ("bot", String "vk")
    , ( "start_request"
      , (Object . HM.fromList)
          [ ( "path"
            , String "https://api.vk.com/method/groups.getLongPollServer")
          , ("params", (Array . V.fromList) ["group_id", "access_token", "v"])
          , ("got", String "response")
          ])
    , ("random_id", Number 0)
    , ("message", String "")
    , ("msgField", String "message")
    , ("attachment", String "")
    , ( "access_token"
      , String
          "d044ae47cea77daa95aa6e5a49fe7fbe5c5deb1fdf54eaf13b23c8d5f88fb277de05702378262ef53d1b3")
    , ( "keyboard"
      , (Object . HM.fromList)
          [ ( "keyboard"
            , String
                "{\"one_time\":true,\"buttons\":[[{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"1\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"2\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"3\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"4\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"5\"},\"color\":\"primary\"}]]}")
          ])
    , ("peer_id", Number 200000000099)
    , ("group_id", Number 152071194)
    , ("v", String "5.103")
    , ("repeatN", Number 1)
    , ( "repeatMsg"
      , String
          "At the moment, I repeat what you said times. Press the button with the number, with the desired number of repetitions.")
    , ( "helpMsg"
      , String
          "Hey. I am a simple echo-bot - I write back what they wrote to me. If you want to change how many times I reply to one of your messages, then write /repeat ")
    , ("logLevel", String "DEBUG")
    ]
