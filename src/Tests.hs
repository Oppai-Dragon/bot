module Tests
    ( runTest
    ) where

import Bot.Vk.Test
import Bot.Telegram.Test
import Config.Test
import Config.Get.Test

import Test.HUnit

runTest = runTestTT
    $ TestList
    $ botVkTests
    <> botTelegramTests
    <> configTests
    <> configGetTests