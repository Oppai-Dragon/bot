module Log.Methods
  ( logM
  , debugM
  , infoM
  , warningM
  , errorM
  ) where

import Log

import Debug.Trace

-----------------------------------Basic---------------------------------------------------
logM -- Log a message using the given logger at a given priority
 ::
     String -- Name of the logger to use
  -> Priority -- Priority of this message
  -> String -- The log text itself
  -> IO ()
logM nameLog priority text =
  traceIO $ "[" <> show priority <> "] " <> nameLog <> text

------------------------------Utility Functions--------------------------------------------
debugM, infoM, warningM, errorM :: String -> String -> IO ()
debugM = (`logM` DEBUG)

infoM = (`logM` INFO)

warningM = (`logM` WARNING)

errorM = (`logM` ERROR)
