module Tests.Config.Get
  ( configGetTests
  ) where

import Config
import Config.Get

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Test.HUnit

configGetTests :: [Test]
configGetTests =
  [ TestLabel "getRequestObjTest" getRequestObjTest
  , TestLabel "getRequestPathTest" getRequestPathTest
  , TestLabel "getRequestParamsTest" getRequestParamsTest
  , TestLabel "valueToIntegerTest" valueToIntegerTest
  , TestLabel "parseRequestPathTest" parseRequestPathTest
  , TestLabel "getUnpackFieldTest" getUnpackFieldTest
  , TestLabel "getKeyboardTest" getKeyboardTest
  , TestLabel "getRepeatMsgTest" getRepeatMsgTest
  ]

getRequestObjTest, getRequestPathTest, getRequestParamsTest, valueToIntegerTest, parseRequestPathTest, getUnpackFieldTest, getKeyboardTest, getRepeatMsgTest ::
     Test
getRequestObjTest =
  TestCase $
  assertEqual
    "for (getRequestObj \"start_request\" testConfig)"
    testStartRequestConfig $
  getRequestObj "start_request" testConfig

getRequestPathTest =
  TestCase $
  assertEqual
    "for (getRequestPath testStartRequestConfig)"
    "https://api.vk.com/method/groups.getLongPollServer" $
  getRequestPath testStartRequestConfig

getRequestParamsTest =
  TestCase $
  assertEqual
    "for (getRequestParams testStartRequestConfig)"
    ["group_id", "access_token", "v"] $
  getRequestParams testStartRequestConfig

valueToIntegerTest =
  TestCase $
  assertEqual "for (valueToInteger (Number 1))" 1 $ valueToInteger (Number 1)

parseRequestPathTest =
  TestCase $
  assertEqual
    "for (parseRequestPath \"<logLevel>\" testConfig)"
    "beforedebugafter" $
  parseRequestPath "before<logLevel>after" testConfig

getUnpackFieldTest =
  TestCase $
  assertEqual "for (getUnpackField \"start_request\" testConfig)" "response" $
  getUnpackField "start_request" testConfig

getKeyboardTest =
  TestCase $
  assertEqual
    "for (getKeyboard testConfig)"
    [ ( "keyboard"
      , String
          "{\"one_time\":true,\"buttons\":[[{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"1\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"2\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"3\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"4\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"5\"},\"color\":\"primary\"}]]}")
    ] $
  getKeyboard testConfig

getRepeatMsgTest =
  TestCase $
  assertEqual
    "for (getRepeatMsg testConfig)"
    "At the moment, I repeat what you said 1 times. Press the button with the number, with the desired number of repetitions." $
  getRepeatMsg testConfig

testStartRequestConfig :: Object
testStartRequestConfig =
  HM.fromList
    [ ("path", String "https://api.vk.com/method/groups.getLongPollServer")
    , ("params", (Array . V.fromList) ["group_id", "access_token", "v"])
    , ("got", String "response")
    ]
