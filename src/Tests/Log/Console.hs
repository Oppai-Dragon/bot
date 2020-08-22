module Tests.Log.Console
  ( logConsoleTests
  ) where

import Log.Console
import Log.Level

import GHC.Stack

import Test.HUnit

logConsoleTests :: [Test]
logConsoleTests =
  [ TestLabel "prettyLogTest" prettyLogTest
  , TestLabel "prettyFileLogTest" prettyFileLogTest
  ]

prettyLogTest, prettyFileLogTest :: Test
prettyLogTest =
  TestCase . assertEqual "for (prettyLog WARNING \"danger\")" "[WARNING] danger" $
  prettyLog WARNING "danger"

prettyFileLogTest =
  TestCase .
  assertEqual
    "for (prettyFileLog testCallStack)"
    "Called at Set.hs 29:17-29:42 in Config.Set" $
  prettyFileLog testCallStack

testCallStack :: CallStack
testCallStack =
  fromCallSiteList
    [ ( "errorM"
      , SrcLoc
          { srcLocPackage = "main"
          , srcLocModule = "Config.Set"
          , srcLocFile = "Set.hs"
          , srcLocStartLine = 29
          , srcLocStartCol = 17
          , srcLocEndLine = 29
          , srcLocEndCol = 42
          })
    ]
