module Bot.Vk.Test
  ( botVkTests
  ) where

import Bot
import Bot.Vk
import Config

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import System.IO.Unsafe (unsafePerformIO)

import Test.HUnit

botVkTests :: [Test]
botVkTests =
  [ TestLabel "getKeysVkTest" getKeysVkTest
  , TestLabel "updateVkTest" updateVkTest
  , TestLabel "getAttachmentTest" getAttachmentTest
  , TestLabel "getVkMsgTest" getVkMsgTest
  ]

getKeysVkTest, updateVkTest, getAttachmentTest, getVkMsgTest :: Test
getKeysVkTest =
  TestCase $
  assertEqual "for (getKeysVk testUpdatesObj)" testUpdatesObj $
  fst $
  unsafePerformIO $
  runStateT (runReaderT (getKeysVk testUpdatesObj) Vk) testConfig

updateVkTest =
  TestCase $
  assertEqual "for (updateVk)" "" $
  fst $
  unsafePerformIO $
  runStateT (runReaderT updateVk (testUpdatesObj, testResponseObj)) testConfig

getAttachmentTest =
  TestCase $
  assertEqual "for (getAttachment [attachmentsObj])" ",audio174435367_456241022" $
  getAttachment [attachmentsObj]

getVkMsgTest =
  TestCase $
  assertEqual "for (getVkMsg)" "" . fst . unsafePerformIO $
  runStateT (runReaderT getVkMsg testUpdatesObj) testConfig

testResponseObj, testUpdatesObj, attachmentsObj :: Object
testResponseObj =
  HM.fromList [("ts", Number 1), ("updates", Object testUpdatesObj)]

testUpdatesObj =
  HM.fromList
    [ ( "object"
      , Object
          (HM.fromList
             [ ( "message"
               , Object
                   (HM.fromList
                      [ ( "attachments"
                        , (Array . V.fromList) [Object attachmentsObj])
                      , ("text", String "")
                      , ("peer_id", Number 1.74435367e8)
                      , ("conversation_message_id", Number 1643.0)
                      , ("random_id", Number 0.0)
                      , ("date", Number 1.594990603e9)
                      , ("from_id", Number 1.74435367e8)
                      , ("is_hidden", Bool False)
                      , ("fwd_messages", Array V.empty)
                      , ("id", Number 1722.0)
                      , ("important", Bool False)
                      , ("out", Number 0.0)
                      ]))
             ]))
    , ("group_id", Number 1.52071194e8)
    , ("type", String "message_new")
    , ("event_id", String "81823d9a3a91026138883249450d9f281bbe2ad3")
    ]

attachmentsObj =
  HM.fromList
    [ ("type", String "audio")
    , ( "audio"
      , Object
          (HM.fromList
             [ ( "url"
               , String
                   "https://cs9-21v4.vkuseraudio.net/p5/899903aa90615c.mp3?extra=wY1ebEmN2-sMSC_w4FX3IEIQVUdj7fG0hDR83Ae8wv1C45z8RB5rIV2PEuYRNf_Ek0E6iIg6pCjmfuek27On69bZJLeJa6u_2jEFtM9V4ne-v8bQcK9pISBWkgnM9Kh4UyLDAq21H59n9yKqiiUGRdY&long_chunk=1")
             , ( "main_artists"
               , (Array . V.fromList)
                   [ Object
                       (HM.fromList
                          [ ("domain", String "8333364158186615256")
                          , ("name", String "Stroke 9")
                          , ("id", String "8333364158186615256")
                          ])
                   ])
             , ("date", Number 1.592947395e9)
             , ("stories_allowed", Bool False)
             , ("id", Number 4.56241022e8)
             , ("track_code", String "96954ffdNHF4tSqDZAh3OYQXuYDuWqR0fR4")
             , ("short_videos_allowed", Bool False)
             , ("title", String "Kick Some Ass")
             , ("is_explicit", Bool False)
             , ("owner_id", Number 1.74435367e8)
             , ("duration", Number 240.0)
             , ("artist", String "Stroke 9")
             , ("is_focus_track", Bool False)
             ]))
    ]
