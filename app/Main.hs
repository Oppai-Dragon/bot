module Main where

import Base
import Config
import Session

main :: IO ()
main = do
  maybeHandle <- Config.maybeNew
  case maybeHandle of
    Just handle -> do
      _ <- runApp runBot handle
      return ()
    Nothing -> return ()