module Tests.Config
  ( testVkHandle
  , testTelegramHandle
  , testLogHandle
  , testVkConfig
  , testTelegramConfig
  ) where

import Base
import Bot
import Config
import Log

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

testVkHandle, testTelegramHandle :: Config.Handle
testVkHandle =
  Config.Handle {hConfig = testVkConfig, hBot = Vk, hLog = testLogHandle}

testTelegramHandle =
  Config.Handle
    {hConfig = testTelegramConfig, hBot = Telegram, hLog = testLogHandle}

testLogHandle :: Log.Handle
testLogHandle =
  Log.Handle
    {hLogPath = setPath "/Test log.txt", hMaybeLogLevel = Just DEBUG}

testTelegramConfig, testVkConfig :: Config
testTelegramConfig =
  HM.fromList
    [ ( "attachments"
      , A.Array $
        V.fromList
          [ A.String "document"
          , A.String "audio"
          , A.String "voice"
          , A.String "sticker"
          , A.String "animation"
          , A.String "photo"
          , A.String "video"
          ])
    , ( "send_request"
      , A.Object
          (HM.fromList
             [ ( "path"
               , A.String
                   "https://api.telegram.org/bot<access_token>/send<method>")
             , ( "params"
               , A.Array $
                 V.fromList
                   [ A.Object (HM.fromList [("chat_id", A.String "chat_id")])
                   , A.Object (HM.fromList [("text", A.String "text")])
                   , A.Object (HM.fromList [("document", A.String "file_id")])
                   , A.Object (HM.fromList [("audio", A.String "file_id")])
                   , A.Object (HM.fromList [("voice", A.String "file_id")])
                   , A.Object (HM.fromList [("sticker", A.String "file_id")])
                   , A.Object (HM.fromList [("animation", A.String "file_id")])
                   , A.Object (HM.fromList [("photo", A.String "file_id")])
                   ])
             ]))
    , ("logLevel", A.String "debug")
    , ( "access_token"
      , A.String "1222090060:AAG110wvYURl-eheQ2eIyDCSWaY3KWxve0s")
    , ( "start_request"
      , A.Object
          (HM.fromList
             [ ( "path"
               , A.String
                   "https://api.telegram.org/bot<access_token>/getUpdates")
             , ("params", A.Array $ V.fromList [A.String "timeout"])
             , ("got", A.String "result")
             ]))
    , ("test", A.String "test")
    , ("bot", A.String "Telegram")
    , ("method", A.String "Message")
    , ( "ask_request"
      , A.Object
          (HM.fromList
             [ ( "path"
               , A.String
                   "https://api.telegram.org/bot<access_token>/getUpdates")
             , ( "params"
               , A.Array $ V.fromList [A.String "timeout", A.String "offset"])
             , ("got", A.String "result")
             ]))
    , ( "keyboard"
      , A.Object
          (HM.fromList
             [ ( "reply_markup"
               , A.String
                   "{\"keyboard\":[[{\"text\":\"1\"},{\"text\":\"2\"},{\"text\":\"3\"},{\"text\":\"4\"},{\"text\":\"5\"}]],\"resize_keyboard\":true,\"one_time_keyboard\":true}")
             ]))
    , ( "helpMsg"
      , A.String
          "Hey. I am a simple echo-bot - I write back what they wrote to me. If you want to change how many times I reply to one of your messages, then write /repeat ")
    , ("msgField", A.String "text")
    , ("timeout", A.Number 30.0)
    , ( "repeatMsg"
      , A.String
          "At the moment, I repeat what you said times. Press the button with the A.number, with the desired A.number of repetitions.")
    , ("repeatN", A.Number 1.0)
    ]

testVkConfig =
  HM.fromList
    [ ( "send_request"
      , A.Object
          (HM.fromList
             [ ("path", A.String "https://api.vk.com/method/messages.send")
             , ( "params"
               , A.Array $
                 V.fromList
                   [ A.String "random_id"
                   , A.String "peer_id"
                   , A.String "message"
                   , A.String "attachment"
                   , A.String "forward_messages"
                   , A.String "access_token"
                   , A.String "v"
                   ])
             ]))
    , ("logLevel", A.String "debug")
    , ( "access_token"
      , A.String
          "d3cfb010771408ed9bf218b4a66df01c33e533b20366a58edfe76e5d5be3f27429c369ecf86ef2a8b6949")
    , ("wait", A.Number 30.0)
    , ( "api_methods"
      , A.Object
          (HM.fromList
             [ ( "docs.save"
               , A.Object
                   (HM.fromList
                      [("params", A.Array $ V.fromList [A.String "file"])]))
             , ( "docs.getMessagesUploadServer"
               , A.Object
                   (HM.fromList
                      [ ( "params"
                        , A.Array $
                          V.fromList [A.String "type", A.String "peer_id"])
                      ]))
             , ( "photos.getMessagesUploadServer"
               , A.Object
                   (HM.fromList
                      [ ( "query"
                        , A.Object (HM.fromList [("peer_id", A.Number 0.0)]))
                      ]))
             , ( "photos.saveMessagesPhoto"
               , A.Object
                   (HM.fromList
                      [("params", A.Array $ V.fromList [A.String "photo_obj"])]))
             , ( "messages.getByConversationMessageId"
               , A.Object
                   (HM.fromList
                      [ ( "params"
                        , A.Array $
                          V.fromList
                            [ A.String "peer_id"
                            , A.String "conversation_message_ids"
                            ])
                      ]))
             ]))
    , ("mode", A.Number 2.0)
    , ( "start_request"
      , A.Object
          (HM.fromList
             [ ( "path"
               , A.String "https://api.vk.com/method/groups.getLongPollServer")
             , ( "params"
               , A.Array $
                 V.fromList
                   [A.String "group_id", A.String "access_token", A.String "v"])
             , ("got", A.String "response")
             ]))
    , ("test", A.String "test")
    , ("act", A.String "a_check")
    , ("bot", A.String "Vk")
    , ( "ask_request"
      , A.Object
          (HM.fromList
             [ ("path", A.String "<server>")
             , ( "params"
               , A.Array $
                 V.fromList
                   [ A.String "act"
                   , A.String "key"
                   , A.String "wait"
                   , A.String "mode"
                   , A.String "ts"
                   ])
             , ("got", A.String "updates")
             ]))
    , ( "keyboard"
      , A.Object
          (HM.fromList
             [ ( "keyboard"
               , A.String
                   "{\"one_time\":true,\"buttons\":[[{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"1\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"2\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"3\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"4\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"5\"},\"color\":\"primary\"}]]}")
             ]))
    , ("group_id", A.Number 1.99167319e8)
    , ( "helpMsg"
      , A.String
          "Hey. I am a simple echo-bot - I write back what they wrote to me. If you want to change how many times I reply to one of your messages, then write /repeat ")
    , ("msgField", A.String "message")
    , ("v", A.String "5.103")
    , ( "repeatMsg"
      , A.String
          "At the moment, I repeat what you said times. Press the button with the A.number, with the desired A.number of repetitions.")
    , ( "api_request"
      , A.Object
          (HM.fromList
             [ ("path", A.String "https://api.vk.com/method/")
             , ( "params"
               , A.Array $ V.fromList [A.String "access_token", A.String "v"])
             ]))
    , ("repeatN", A.Number 1.0)
    ]
