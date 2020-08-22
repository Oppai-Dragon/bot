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
import qualified Control.Monad.Trans.Class as MonadClass
import qualified Control.Monad.Trans.Reader as MonadR
import qualified Control.Monad.Trans.State.Strict as MonadSS

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.List as L
import qualified Data.Text as T

import qualified System.Directory as Dir
import qualified System.Random as Random
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format.ISO8601 as TimeFormat

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
  utcTime <- Time.getCurrentTime
  let utcTimeStr = TimeFormat.iso8601Show utcTime
  let time = L.takeWhile (/='.') . tail $ L.dropWhile (/='T') utcTimeStr
  return time

fromApp ::
     MonadClass.MonadTrans t
  => MonadSS.StateT s IO a
  -> t (MonadSS.StateT s IO) a
fromApp = MonadClass.lift

fromIO :: MonadClass.MonadTrans t => IO a -> t IO a
fromIO = MonadClass.lift

askApp :: Monad m => MonadR.ReaderT r m r
askApp = MonadR.ask

getApp :: Monad m => MonadSS.StateT s m s
getApp = MonadSS.get

putApp :: Monad m => s -> MonadSS.StateT s m ()
putApp = MonadSS.put

runRApp :: Monad m => MonadR.ReaderT b m a -> b -> m a
runRApp = MonadR.runReaderT

runSApp :: Monad m => MonadSS.StateT s m a -> s -> m a
runSApp = MonadSS.evalStateT

runApp :: Monad m => MonadSS.StateT s m a -> s -> m (a, s)
runApp = MonadSS.runStateT
