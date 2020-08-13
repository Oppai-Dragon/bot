module Main where

import Config (setConfig)
import Lib (runBot)

import Control.Monad.Trans.State.Strict (runStateT)

main :: IO ()
main = do
  config <- setConfig
  (a, _) <- runStateT runBot config
  return a
