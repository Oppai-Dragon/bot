module Log.Handle
  ( Handle(..)
  ) where

import Log.Level

data Handle =
  Handle
    { hLogPath :: FilePath
    , hMaybeLogLevel :: Maybe Level
    }
  deriving (Show,Eq)
