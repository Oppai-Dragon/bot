module Tests
  ( runTest
  ) where

import Bot.Telegram.Test
import Bot.Vk.Test
import Config.Get.Test
import Config.Test

import Test.HUnit

runTest :: IO Counts
runTest =
  runTestTT . TestList $
  botVkTests <> botTelegramTests <> configTests <> configGetTests
