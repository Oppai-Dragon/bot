module Main where

import Base
import Config
import Log
import Session

import Control.Monad.Trans.Maybe (MaybeT(..))
import System.IO (hClose)

main :: IO ()
main = do
  maybeHandle <- runMaybeT Config.maybeNew
  case maybeHandle of
    Just handle -> do
      _ <- runApp runBot handle
      hClose . hLogFileHandle $ hConfigLogHandle handle
    Nothing -> return ()
