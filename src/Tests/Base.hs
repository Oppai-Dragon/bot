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
  ]

parsePathTest, getValueTest :: Test
parsePathTest =
  TestCase $
  assertEqual "for (parsePath \"E:\\users\\get\\src\")" "E:\\users\\get" $
  parsePath "E:\\users\\get\\src"

getValueTest =
  TestCase $
  assertEqual "for (getValue [\"repeatN\"] testConfig)" (A.Number 1) $
  getValue ["repeatN"] testConfig
