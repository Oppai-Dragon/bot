module Tests.Bot.Vk
  ( botVkTests
  ) where

import Base
import Bot.Vk
import Config
import Log

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Test.HUnit

botVkTests :: [Test]
botVkTests =
  [ TestLabel "updateTest" updateTest
  , TestLabel "getAttachmentTest" getAttachmentTest
  , TestLabel "getMsgTest" getMsgTest
  ]

updateTest, getAttachmentTest, getMsgTest :: Test
updateTest =
  TestCase $
  runApp (runSubApp (update testResponseObj) testUpdatesObj) testHandle >>=
  assertEqual
    "for (runStateT (runReaderT (update testResponseObj) testUpdatesObj) testHandle"
    ("privet", testUpdatedHandle)

getAttachmentTest =
  TestCase $
  assertEqual "for (getAttachment [attachmentsObj])" ",audio174435367_456241022" $
  getAttachment [attachmentsObj]

getMsgTest =
  TestCase $
  evalApp (runSubApp getMsg testUpdatesObj) testHandle >>=
  assertEqual
    "for (evalApp (runSubApp getMsg testUpdatesObj) testHandle >>= \\(a,_) -> return a)"
    "privet"

testUpdatedHandle :: Config.Handle
testUpdatedHandle = Config.Handle testUpdatedConfig (Log.Handle "" Nothing)

testResponseObj, testUpdatedConfig, testUpdated, testUpdatesObj, attachmentsObj ::
     A.Object
testResponseObj =
  HM.fromList [("updates", A.Object testUpdatesObj), ("ts", A.Number 1)]

testUpdatedConfig = HM.union testUpdated testConfig

testUpdated =
  HM.fromList
    [ ("ts", A.Number 1)
    , ("user_id", A.Number 1.74435367e8)
    , ("attachment", A.String "audio174435367_456241022")
    ]

testUpdatesObj =
  HM.fromList
    [ ( "object"
      , A.Object
          (HM.fromList
             [ ( "message"
               , A.Object
                   (HM.fromList
                      [ ( "attachments"
                        , (A.Array . V.fromList) [A.Object attachmentsObj])
                      , ("text", A.String "privet")
                      , ("peer_id", A.Number 1.74435367e8)
                      , ("conversation_message_id", A.Number 1643.0)
                      , ("random_id", A.Number 0.0)
                      , ("date", A.Number 1.594990603e9)
                      , ("from_id", A.Number 1.74435367e8)
                      , ("is_hidden", A.Bool False)
                      , ("fwd_messages", A.Array V.empty)
                      , ("id", A.Number 1722.0)
                      , ("important", A.Bool False)
                      , ("out", A.Number 0.0)
                      ]))
             ]))
    , ("group_id", A.Number 1.52071194e8)
    , ("type", A.String "message_new")
    , ("event_id", A.String "81823d9a3a91026138883249450d9f281bbe2ad3")
    ]

attachmentsObj =
  HM.fromList
    [ ("type", A.String "audio")
    , ( "audio"
      , A.Object
          (HM.fromList
             [ ( "url"
               , A.String
                   "https://cs9-21v4.vkuseraudio.net/p5/899903aa90615c.mp3?extra=wY1ebEmN2-sMSC_w4FX3IEIQVUdj7fG0hDR83Ae8wv1C45z8RB5rIV2PEuYRNf_Ek0E6iIg6pCjmfuek27On69bZJLeJa6u_2jEFtM9V4ne-v8bQcK9pISBWkgnM9Kh4UyLDAq21H59n9yKqiiUGRdY&long_chunk=1")
             , ( "main_artists"
               , (A.Array . V.fromList)
                   [ A.Object
                       (HM.fromList
                          [ ("domain", A.String "8333364158186615256")
                          , ("name", A.String "Stroke 9")
                          , ("id", A.String "8333364158186615256")
                          ])
                   ])
             , ("date", A.Number 1.592947395e9)
             , ("stories_allowed", A.Bool False)
             , ("id", A.Number 4.56241022e8)
             , ("track_code", A.String "96954ffdNHF4tSqDZAh3OYQXuYDuWqR0fR4")
             , ("short_videos_allowed", A.Bool False)
             , ("title", A.String "Kick Some Ass")
             , ("is_explicit", A.Bool False)
             , ("owner_id", A.Number 1.74435367e8)
             , ("duration", A.Number 240.0)
             , ("artist", A.String "Stroke 9")
             , ("is_focus_track", A.Bool False)
             ]))
    ]
