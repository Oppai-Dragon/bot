module Tests.Base.Prelude
  ( basePreludeTests
  ) where

import Base.Prelude

import Test.HUnit

basePreludeTests :: [Test]
basePreludeTests = [TestLabel "wordsByTest" wordsByTest]

wordsByTest :: Test
wordsByTest =
  TestCase $
  assertEqual "for (wordsBy (\'.\'/=) \"kek.wew\")" ["kek", "wew"] $
  wordsBy ('.' /=) "kek.wew"
