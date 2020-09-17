module Tests.Config.Update
  ( configUpdateTests
  ) where

import Base
import Config
import Config.Update

import Test.HUnit

configUpdateTests :: [Test]
configUpdateTests = [TestLabel "parseMessageTest" parseMessageTest]

parseMessageTest :: Test
parseMessageTest =
  TestCase $
  evalApp (parseMessage "[club152071194|@club152071194] 3") testHandle >>=
  assertEqual "for (parseMessage \"[club152071194|@club152071194] 3\")" "3"
