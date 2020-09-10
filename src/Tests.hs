module Tests
  ( runTest
  ) where

import Tests.Base
import Tests.Bot.Telegram
import Tests.Bot.Vk
import Tests.Config.Get

import Test.HUnit

runTest :: IO Counts
runTest =
  runTestTT . TestList $
  botVkTests <> botTelegramTests <> baseTests <> configGetTests
