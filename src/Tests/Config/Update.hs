module Tests.Config.Update
  ( configUpdateTests
  ) where

import Base
import Config.Update
import Tests.Config

import Test.HUnit

configUpdateTests :: [Test]
configUpdateTests = [TestLabel "parseMessageTest" parseMessageTest]

parseMessageTest :: Test
parseMessageTest =
  TestCase $
  evalApp (parseMessage "[club152071194|@club152071194] 3") testVkHandle >>=
  assertEqual
    "for (evalApp (parseMessage \"[club152071194|@club152071194] 3\") testVkHandle >>=)"
    "3"
