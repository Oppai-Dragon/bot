module Tests.Bot.Telegram
  ( botTelegramTests
  ) where

import Base
import Bot
import Bot.Telegram
import Config
import Log

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
  runApp (runRApp (getKeys testUpdatesObj) Telegram) testHandle >>= \(a, _) ->
    assertEqual
      "for (runApp (runRApp (getKeys testUpdatesObj) Telegram) testHandle >>= \\(a, _) -> return a)"
      (HM.singleton "chat_id" (A.Number 1))
      a

updateTest =
  TestCase $
  runApp (runRApp update testUpdatesObj) testHandle >>=
  assertEqual
    "for (runApp (runRApp update testUpdatesObj) testHandle)"
    ("suka", testUpdatedHandle)

getMsgTest =
  TestCase $
  runApp (runRApp getMsg testUpdatesObj) testHandle >>= \(a, _) ->
    assertEqual
      "for (runApp (runRApp getMsg testUpdatesObj) testHandle >>= \\(a,_) -> return a)"
      "suka"
      a

testUpdatedHandle :: Config.Handle
testUpdatedHandle = Config.Handle testUpdatedConfig (Log.Handle "" Nothing)

testUpdatedConfig, testUpdated, testUpdatesObj :: A.Object
testUpdatedConfig = HM.union testUpdated testConfig

testUpdated =
  HM.fromList [("offset", A.Number 6.26040329e8), ("chat_id", A.Number 1)]

testUpdatesObj =
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
