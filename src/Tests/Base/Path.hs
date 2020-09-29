module Tests.Base.Path
  (basePathTests) where

import Base.Path

import Test.HUnit

basePathTests :: [Test]
basePathTests =
  [ TestLabel "parsePathTest" parsePathTest
  ]

parsePathTest :: Test
parsePathTest =
  TestCase $
  assertEqual "for (parsePath \"E:\\users\\get\\src\")" "E:\\users\\get" $
  parsePath "E:\\users\\get\\src"