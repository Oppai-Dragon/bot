module LogConfig (logMsg) where

import Log (Priority (..))

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