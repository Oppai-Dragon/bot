module Base.Lift
  ( liftApp
  , liftIO
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

liftApp :: MonadTrans t => StateT s IO a -> t (StateT s IO) a
liftApp = lift
