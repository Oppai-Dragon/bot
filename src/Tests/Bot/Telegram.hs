module Tests.Bot.Telegram
  ( botTelegramTests
  ) where

import Base
import Bot.Telegram
import Config
import Tests.Config

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

import Test.HUnit

botTelegramTests :: [Test]
botTelegramTests =
  [ TestLabel "getKeysTest" getKeysTest
  , TestLabel "updateTest" updateTest
  , TestLabel "getMsgTest" getMsgTest
  ]

getKeysTest, updateTest, getMsgTest :: Test
getKeysTest =
  TestCase $
  evalApp (getKeys testUpdatesTextObj) testTelegramHandle >>=
  assertEqual
    "for (evalApp (getKeys testUpdatesTextObj) testTelegramHandle)"
    (HM.singleton "chat_id" (A.Number 1))

updateTest =
  TestCase $
  runApp (runSubApp update testUpdatesDocumentObj) testTelegramHandle >>=
  assertEqual
    "for (runApp (runSubApp update testUpdatesDocumentObj) testTelegramHandle)"
    ("", testUpdatedHandle)

getMsgTest =
  TestCase $
  evalApp (runSubApp getMsg testUpdatesTextObj) testTelegramHandle >>=
  assertEqual
    "for (evalApp (runSubApp getMsg testUpdatesTextObj) testTelegramHandle)"
    "suka"

testUpdatedHandle :: Config.Handle
testUpdatedHandle = testTelegramHandle {hConfig = testUpdatedConfig}

testUpdatedConfig, testUpdated, testUpdatesDocumentObj, testUpdatesTextObj ::
     A.Object
testUpdatedConfig = HM.union testUpdated testTelegramConfig

testUpdated =
  HM.fromList
    [ ("offset", A.Number 6.26040329e8)
    , ("chat_id", A.Number 1.09778397e9)
    , ("method", A.String "Document")
    , ( "file_id"
      , A.String
          "BQACAgIAAxkBAAICUV9ZJD1sYB1qKT8Y_IX5-_G8i4RqAAJRCgAC0-7JSuwAAZkJvTsUvRsE")
    ]

testUpdatesDocumentObj =
  HM.fromList
    [ ("update_id", A.Number 6.26040329e8)
    , ( "message"
      , A.Object
          (HM.fromList
             [ ( "from"
               , A.Object
                   (HM.fromList
                      [ ("first_name", A.String "Misha")
                      , ("is_bot", A.Bool False)
                      , ("last_name", A.String "Dragon")
                      , ("id", A.Number 1.09778397e9)
                      , ("language_code", A.String "ru")
                      ]))
             , ( "chat"
               , A.Object
                   (HM.fromList
                      [ ("first_name", A.String "Misha")
                      , ("last_name", A.String "Dragon")
                      , ("id", A.Number 1.09778397e9)
                      , ("type", A.String "private")
                      ]))
             , ("message_id", A.Number 593.0)
             , ("date", A.Number 1.599677501e9)
             , ( "document"
               , A.Object
                   (HM.fromList
                      [ ( "file_id"
                        , A.String
                            "BQACAgIAAxkBAAICUV9ZJD1sYB1qKT8Y_IX5-_G8i4RqAAJRCgAC0-7JSuwAAZkJvTsUvRsE")
                      , ("mime_type", A.String "image/jpeg")
                      , ( "thumb"
                        , A.Object
                            (HM.fromList
                               [ ( "file_id"
                                 , A.String
                                     "AAMCAgADGQEAAgJRX1kkPWxgHWopPxj8hfn78byLhGoAAlEKAALT7slK7AABmQm9OxS9vFV_ly4AAwEAB20AA2dxAAIbBA")
                               , ("height", A.Number 209.0)
                               , ("width", A.Number 220.0)
                               , ("file_size", A.Number 20867.0)
                               , ( "file_unique_id"
                                 , A.String "AQADvFV_ly4AA2dxAAI")
                               ]))
                      , ("file_size", A.Number 31399.0)
                      , ("file_unique_id", A.String "AgADUQoAAtPuyUo")
                      , ("file_name", A.String "220px-Pet_Hamster.jpg")
                      ]))
             ]))
    ]

testUpdatesTextObj =
  HM.fromList
    [ ("update_id", A.Number 6.26040329e8)
    , ( "message"
      , A.Object
          (HM.fromList
             [ ("text", A.String "suka")
             , ( "from"
               , A.Object
                   (HM.fromList
                      [ ("first_name", A.String "Misha")
                      , ("is_bot", A.Bool False)
                      , ("last_name", A.String "Dragon")
                      , ("id", A.Number 1.09778397e9)
                      , ("language_code", A.String "ru")
                      ]))
             , ( "chat"
               , A.Object
                   (HM.fromList
                      [ ("first_name", A.String "Misha")
                      , ("last_name", A.String "Dragon")
                      , ("id", A.Number 1)
                      , ("type", A.String "private")
                      ]))
             , ("message_id", A.Number 464.0)
             , ("date", A.Number 1.594995148e9)
             ]))
    ]
