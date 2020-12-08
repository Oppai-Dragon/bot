module Main where

import Base
import Config
import Session

import Control.Monad.Trans.Maybe (MaybeT(..))

main :: IO ()
main = do
  maybeHandle <- runMaybeT Config.maybeNew
  case maybeHandle of
    Just handle -> do
      _ <- runApp runBot handle
      return ()
    Nothing -> return ()