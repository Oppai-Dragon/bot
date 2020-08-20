module Main where

import Base
import Config
import Session

main :: IO ()
main = do
  handle <- Config.new
  (a, _) <- runApp runBot handle
  return a