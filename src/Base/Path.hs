{-# LANGUAGE LambdaCase #-}

module Base.Path
  ( parsePath
  , getRepDir
  ) where

import Data.List
import qualified System.Directory as Dir

parsePath :: FilePath -> FilePath
parsePath =
  intercalate "\\" .
  takeWhile (/= "src") .
  words .
  intercalate "" .
  map
    (\case
       "\\" -> " "
       x -> x) .
  group

getRepDir :: IO FilePath
getRepDir = parsePath <$> Dir.getCurrentDirectory
