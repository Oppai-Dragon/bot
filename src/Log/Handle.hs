module Log.Handle
  ( Handle(..)
  , defaultHandle
  ) where

import Log.Level

import qualified System.IO as IO

data Handle =
  Handle
    { hLogFileHandle :: IO.Handle
    , hLogMaybeLevel :: Maybe Level
    }
  deriving (Show,Eq)

defaultHandle :: Handle
defaultHandle = Handle {hLogMaybeLevel = Nothing, hLogFileHandle = IO.stdout}
