module Config.Test
  ( configTests
  ) where

import Config

import System.Directory (getCurrentDirectory)
import System.IO.Unsafe (unsafePerformIO)

import Test.HUnit

configTests :: [Test]
configTests =
  [ TestLabel "parsePathTest" parsePathTest
  , TestLabel "setPathTest" setPathTest
  ]

parsePathTest, setPathTest :: Test

parsePathTest =
  TestCase $
  assertEqual "for (parsePath \"E:\\users\\get\\src\")" "E:\\users\\get" $
  unsafePerformIO parsePath "E:\\users\\get\\src"

setPathTest =
  TestCase $
  assertEqual
    "for (setPath \"Config.json\")"
    (unsafePerformIO getCurrentDirectory <> "\\src\\Config.json") $
  (unsafePerformIO . setPath) "Config.json"