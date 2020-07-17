module Config.Test
    ( configTests
    ) where

import Config

import System.Directory (getCurrentDirectory)
import System.IO.Unsafe (unsafePerformIO)

import Test.HUnit

configTests =
    [ TestLabel "parsePathTest"     parsePathTest
    , TestLabel "setPathTest"       setPathTest
    , TestLabel "iterateListTest"   iterateListTest
    ]

parsePathTest =
    TestCase $
    assertEqual "for (parsePath \"E:\\users\\get\\src\")"
    "E:\\users\\get"
    $ (unsafePerformIO parsePath) "E:\\users\\get\\src"

setPathTest =
    TestCase $
    assertEqual "for (setPath \"Config.json\")"
    ((unsafePerformIO getCurrentDirectory) <> "\\src\\Config.json")
    $ (unsafePerformIO . setPath) "Config.json"

iterateListTest =
    TestCase $
    assertEqual "for (iterateList (flip const) 1 [3,2,1])"
    1
    $ iterateList (flip const) 1 [3,2,1]