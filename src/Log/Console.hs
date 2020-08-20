module Log.Console
  ( debugM
  , infoM
  , warningM
  , errorM
  ) where

import Log.Level

import Debug.Trace

-------------------------------------------------------------------------------
-- * Basic
logM -- Log a message using the given logger at a given priority
 ::
     String -- Name of the logger to use
  -> Level -- Priority of this message
  -> String -- The log text itself
  -> IO ()
logM nameLog priority text =
  traceIO $ "[" <> show priority <> "] " <> nameLog <> text

-------------------------------------------------------------------------------
-- * Utility Functions
debugM, infoM, warningM, errorM :: String -> String -> IO ()
debugM = (`logM` DEBUG)

infoM = (`logM` INFO)

warningM = (`logM` WARNING)

errorM = (`logM` ERROR)
