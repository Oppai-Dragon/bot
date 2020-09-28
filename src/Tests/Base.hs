module Tests.Base
  ( baseTests
  ) where

import Base
import Config

import qualified Data.Aeson as A

import Test.HUnit

baseTests :: [Test]
baseTests =
  [ TestLabel "parsePathTest" parsePathTest
  , TestLabel "getValueTest" getValueTest
  , TestLabel "wordsByTest" wordsByTest
  ]

parsePathTest, getValueTest, wordsByTest :: Test
parsePathTest =
  TestCase $
  assertEqual "for (parsePath \"E:\\users\\get\\src\")" "E:\\users\\get" $
  parsePath "E:\\users\\get\\src"

getValueTest =
  TestCase $
  assertEqual "for (getValue [\"repeatN\"] testConfig)" (A.Number 1) $
  getValue ["repeatN"] testConfig

wordsByTest =
  TestCase $
  assertEqual "for (wordsBy (\'.\'/=) \"kek.wew\")" ["kek","wew"] $ wordsBy ('.'/=) "kek.wew"