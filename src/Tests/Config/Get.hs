module Tests.Config.Get
  ( configGetTests
  ) where

import Config
import Config.Get

import Test.HUnit

configGetTests :: [Test]
configGetTests = [TestLabel "parseRequestPathTest" parseRequestPathTest]

parseRequestPathTest :: Test
parseRequestPathTest =
  TestCase $
  assertEqual "for (parseRequestPath \"<test>\" testConfig)" "beforetestafter" $
  parseRequestPath "before<test>after" testConfig
