module Tests.Bot.Telegram
  ( botTelegramTests
  ) where

import Bot
import Bot.Telegram
import Config

import Data.Aeson
import qualified Data.HashMap.Strict as HM

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import System.IO.Unsafe (unsafePerformIO)

import Test.HUnit

botTelegramTests :: [Test]
botTelegramTests =
  [ TestLabel "getKeysTelegramTest" getKeysTelegramTest
  , TestLabel "updateTelegramTest" updateTelegramTest
  , TestLabel "getTelegramMsgTest" getTelegramMsgTest
  ]

getKeysTelegramTest, updateTelegramTest, getTelegramMsgTest :: Test

getKeysTelegramTest =
  TestCase .
  assertEqual
    "for (getKeysTelegram testUpdatesObj)"
    (HM.singleton "chat_id" (Number 1)) . fst . unsafePerformIO $
  runStateT (runReaderT (getKeysTelegram testUpdatesObj) Telegram) testConfig

updateTelegramTest =
  TestCase $
  assertEqual "for (updateTelegram )" "suka" $
  fst $
  unsafePerformIO $
  runStateT (runReaderT updateTelegram testUpdatesObj) testConfig

getTelegramMsgTest =
  TestCase $
  assertEqual "for (getTelegramMsg )" "suka" $
  fst $
  unsafePerformIO $
  runStateT (runReaderT getTelegramMsg testUpdatesObj) testConfig

testUpdatesObj :: Object
testUpdatesObj =
  HM.fromList
    [ ("update_id", Number 6.26040329e8)
    , ( "message"
      , Object
          (HM.fromList
             [ ("text", String "suka")
             , ( "from"
               , Object
                   (HM.fromList
                      [ ("first_name", String "Misha")
                      , ("is_bot", Bool False)
                      , ("last_name", String "Dragon")
                      , ("id", Number 1.09778397e9)
                      , ("language_code", String "ru")
                      ]))
             , ( "chat"
               , Object
                   (HM.fromList
                      [ ("first_name", String "Misha")
                      , ("last_name", String "Dragon")
                      , ("id", Number 1)
                      , ("type", String "private")
                      ]))
             , ("message_id", Number 464.0)
             , ("date", Number 1.594995148e9)
             ]))
    ]
