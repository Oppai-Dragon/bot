module Base.Lift
  ( liftApp
  , liftIO
  , liftMaybeT
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State.Strict (StateT)

liftApp :: MonadTrans t => StateT s IO a -> t (StateT s IO) a
liftApp = lift

liftMaybeT :: Control.Monad.IO.Class.MonadIO m => MaybeT IO a -> MaybeT m a
liftMaybeT = MaybeT . liftIO . runMaybeT
