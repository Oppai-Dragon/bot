module Tests
  ( runTest
  ) where

import Tests.Base
import Tests.Bot
import Tests.Config.Get
import Tests.Config.Update
import Tests.Request.Modify

import Test.HUnit

runTest :: IO Counts
runTest =
  runTestTT . TestList $
  baseTests <>
  botTests <> configGetTests <> configUpdateTests <> requestModifyTests
