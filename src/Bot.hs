module Bot
  ( Bot(..)
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T

data Bot
  = Vk
  | Telegram
  deriving (Show, Eq, Read)

instance A.FromJSON Bot where
  parseJSON =
    A.withText "From JSON Bot.Bot" $ \x ->
      case x of
        "Vk" -> pure Vk
        "Telegram" -> pure Telegram
        _ -> fail $ "Unknown bot name: " <> T.unpack x
