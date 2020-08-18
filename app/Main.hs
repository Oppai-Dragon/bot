module Main where

import Config (handle)
import Session (runBot)

import Control.Monad.Trans.State.Strict (runStateT)

main :: IO ()
main = do
  handle <- Config.new
  (a, _) <- runStateT runBot handle
  return a