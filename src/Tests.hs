module Tests
  ( runTest
  ) where

import Log
import Tests.Base
import Tests.Bot.Telegram
import Tests.Bot.Vk
import Tests.Config
import Tests.Config.Get
import Tests.Request.Modify

import Test.HUnit

runTest :: IO Counts
runTest = do
  logHandle <- new
  infoM logHandle "TESTS-------------------------------------"
  runTestTT . TestList $
    botVkTests <> botTelegramTests <> baseTests <> configTests <> configGetTests <> requestModifyTests
