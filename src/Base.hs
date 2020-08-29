{-# LANGUAGE LambdaCase #-}

module Base
  ( parsePath
  , getRepDir
  , getValue
  , getRandomInteger
  , getTime
  , fromApp
  , fromIO
  , askApp
  , getApp
  , putApp
  , runRApp
  , runSApp
  , runApp
  ) where

{-
 This module has functions that may be needed in any module,
therefore the import of modules of this project is strictly prohibited.
-}
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.List as L
import qualified Data.Text as T

import qualified Data.Time.LocalTime as LocalTime
import qualified System.Directory as Dir
import qualified System.Random as Random

parsePath :: FilePath -> FilePath
parsePath =
  L.intercalate "\\" .
  takeWhile (/= "src") .
  L.words .
  L.intercalate "" .
  map
    (\case
       "\\" -> " "
       x -> x) .
  L.group

getRepDir :: IO FilePath
getRepDir = parsePath <$> Dir.getCurrentDirectory

getValue :: [T.Text] -> A.Object -> A.Value
getValue [] obj = A.Object obj
getValue (field:rest) objOld =
  case AT.parseMaybe (A..: field) objOld of
    Just (A.Object objNew) -> getValue rest objNew
    Just value -> value
    Nothing -> A.Null

getRandomInteger :: IO Integer
getRandomInteger = Random.getStdRandom (Random.randomR (1, 100000000000))

getTime :: IO String
getTime = do
  zonedTime <- LocalTime.getZonedTime
  let zonedTimeStr = show zonedTime
  let time = L.takeWhile (/= '.') . tail $ L.dropWhile (/= ' ') zonedTimeStr
  return time

fromApp :: MonadTrans t => StateT s IO a -> t (StateT s IO) a
fromApp = lift

fromIO :: MonadTrans t => IO a -> t IO a
fromIO = lift

askApp :: Monad m => ReaderT r m r
askApp = ask

getApp :: Monad m => StateT s m s
getApp = get

putApp :: Monad m => s -> StateT s m ()
putApp = put

runRApp :: Monad m => ReaderT b m a -> b -> m a
runRApp = runReaderT

runSApp :: Monad m => StateT s m a -> s -> m a
runSApp = evalStateT

runApp :: Monad m => StateT s m a -> s -> m (a, s)
runApp = runStateT
