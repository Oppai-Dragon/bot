module Tests.Config.Get
  ( configGetTests
  ) where

import Config.Get
import Tests.Config

import Test.HUnit

configGetTests :: [Test]
configGetTests = [TestLabel "parseRequestPathTest" parseRequestPathTest]

parseRequestPathTest :: Test
parseRequestPathTest =
  TestCase $
  assertEqual "for (parseRequestPath \"<test>\" testVkConfig)" "beforetestafter" $
  parseRequestPath "before<test>after" testVkConfig
