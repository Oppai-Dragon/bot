module Bot
  ( Bot(..)
  ) where

data Bot
  = Vk
  | Telegram
  deriving (Show, Eq, Read)
