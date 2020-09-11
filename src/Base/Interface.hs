module Base.Interface
  ( askSubApp
  , getApp
  , putApp
  , runSubApp
  , evalApp
  , execApp
  , runApp
  ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

askSubApp :: Monad m => ReaderT r m r
askSubApp = ask

getApp :: Monad m => StateT s m s
getApp = get

putApp :: Monad m => s -> StateT s m ()
putApp = put

runSubApp :: Monad m => ReaderT b m a -> b -> m a
runSubApp = runReaderT

evalApp :: Monad m => StateT s m a -> s -> m a
evalApp = evalStateT

execApp :: Monad m => StateT s m a -> s -> m s
execApp = execStateT

runApp :: Monad m => StateT s m a -> s -> m (a, s)
runApp = runStateT
