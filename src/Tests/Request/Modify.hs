module Tests.Request.Modify
  ( requestModifyTests
  ) where

import Request.Modify

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

import Test.HUnit

requestModifyTests :: [Test]
requestModifyTests =
  [ TestLabel "isNeedKeyboardTest" isNeedKeyboardTest
  , TestLabel "isNeedStickerTest" isNeedStickerTest
  ]

isNeedKeyboardTest, isNeedStickerTest :: Test
isNeedKeyboardTest =
  TestCase $
  assertEqual "for (isNeedKeyboard testConfigObj)" True $
  isNeedKeyboard testConfigObj

isNeedStickerTest =
  TestCase $
  assertEqual "for (isNeedSticker testConfigObj)" True $
  isNeedSticker testConfigObj

testConfigObj :: A.Object
testConfigObj =
  HM.fromList
    [("lastMsg", A.String "/repeat"), ("attachment", A.String "sticker")]
