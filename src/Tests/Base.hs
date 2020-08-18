module Tests.Base
  ( baseTests
  ) where

import Config

import System.Directory (getCurrentDirectory)
import System.IO.Unsafe (unsafePerformIO)

import Test.HUnit

configTests :: [Test]
configTests = [TestLabel "parsePathTest" parsePathTest]

parsePathTest :: Test
parsePathTest =
  TestCase $
  parsePath "E:\\users\\get\\src" >>=
  assertEqual "for (parsePath \"E:\\users\\get\\src\")" "E:\\users\\get"
