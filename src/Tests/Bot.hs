module Tests.Bot
  ( botTests
  ) where

import Tests.Bot.Telegram
import Tests.Bot.Vk

import Test.HUnit

botTests :: [Test]
botTests = botVkTests <> botTelegramTests
