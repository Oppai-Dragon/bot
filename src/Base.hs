{-# LANGUAGE LambdaCase #-}

module Base where

{-
 This module has functions that may be needed in any module,
therefore the import of modules of this project is strictly prohibited.
-}
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.List as L
import qualified System.Directory as Dir
import qualified System.Random as Random

parsePath :: IO (FilePath -> FilePath)
parsePath =
  return
    (L.intercalate "\\" .
     takeWhile (/= "src") .
     L.words .
     L.intercalate "" .
     map
       (\case
          "\\" -> " "
          x -> x) .
     L.group)

getRepDir :: IO FilePath
getRepDir = parsePath <*> Dir.getCurrentDirectory

getValue :: Fields -> Object -> Value
getValue [] obj = Object obj
getValue (field:rest) objOld =
  case parseMaybe (.: field) objOld of
    Just (Object objNew) -> getValue rest objNew
    Just value -> value
    Nothing -> Null

getRandom :: IO Integer
getRandom = Random.getStdRandom (Random.randomR (1, 100000000000))