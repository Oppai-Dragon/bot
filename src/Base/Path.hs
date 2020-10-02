{-# LANGUAGE LambdaCase #-}

module Base.Path
  ( parsePath
  , getRepDir
  , setPath
  ) where

import Data.List
import qualified System.Directory as Dir
import System.IO.Unsafe (unsafePerformIO)

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

setPath :: FilePath -> FilePath
setPath = (<>) (unsafePerformIO getRepDir)