module Bot
  ( Bot(..)
  ) where

import qualified Data.Char as C

data Bot
  = Vk
  | Telegram
  deriving (Show, Eq)

instance Read Bot where
  readsPrec _ input =
    case map C.toUpper input of
      "VK" -> [(Vk, "")]
      "TELEGRAM" -> [(Telegram, "")]
      _ -> []
