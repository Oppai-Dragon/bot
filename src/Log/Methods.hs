module Log.Methods
    ( logM
    , debugM
    , infoM
    , warningM
    , errorM
    , logMsg
    , logMsgs
    ) where

import Log

import Debug.Trace

-----------------------------------Basic---------------------------------------------------
logM  -- Log a message using the given logger at a given priority
    :: String   -- Name of the logger to use
    -> Priority -- Priority of this message
    -> String   -- The log text itself
    -> IO ()
logM nameLog priority text = traceIO $
    "[" <> show priority <> "] " <> nameLog <> text
------------------------------Utility Functions--------------------------------------------
debugM,infoM, warningM, errorM :: String -> String -> IO ()
debugM = \x -> logM x DEBUG
infoM = \x -> logM x INFO
warningM = \x -> logM x WARNING
errorM = \x -> logM x ERROR

logMsg :: String -> (Priority, String)
logMsg event = case lookup event logMsgs of
    Nothing -> (DEBUG,"Config value not found")
    Just val -> val

logMsgs :: [(String,(Priority, String))]
logMsgs = logMsgsINFO
logMsgsINFO = map (\(l,r) -> (l, (INFO,r)))
    [ ("startSession","You are connected to the session, now you can receive answers to requests.")
    , ("keySession","The keys to send requests to the server are received.")
    ]