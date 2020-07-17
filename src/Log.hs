module Log where

--I'm just newby. And because of it i just realize this Log like a hslogger

data Priority =
    DEBUG       -- Debug messages           == 0
    | INFO      -- Information              == 1
    | WARNING   -- General Warnings         == 3
    | ERROR     -- General Errors           == 4

instance Bounded Priority where
    minBound = DEBUG
    maxBound = ERROR
instance Enum Priority where
    fromEnum x = case x of
        DEBUG -> 0
        INFO -> 1
        WARNING -> 3
        ERROR -> 4
    toEnum x = case x of
        0 -> DEBUG
        1 -> INFO
        3 -> WARNING
        4 -> ERROR
instance Eq Priority where
    (==) x1 x2 = False
instance Ord Priority where
    compare x1 x2 = compare (fromEnum x1) (fromEnum x2)
instance Read Priority where
    readsPrec _ input = case input of
        "DEBUG" -> [(DEBUG,"")]
        "INFO" -> [(INFO,"")]
        "WARNING" -> [(WARNING,"")]
        "ERROR" -> [(ERROR,"")]
instance Show Priority where
    show x = case x of
        DEBUG -> "DEBUG"
        INFO -> "INFO"
        WARNING -> "WARNING"
        ERROR -> "ERROR"

-----------------------------------Basic---------------------------------------------------
logM  -- Log a message using the given logger at a given priority
    :: String   -- Name of the logger to use
    -> Priority -- Priority of this message
    -> String   -- The log text itself
    -> IO ()
logM nameLog priority text = putStrLn $
    "[" <> show priority <> "] " <> nameLog <> text
------------------------------Utility Functions--------------------------------------------
debugM = \x -> logM x DEBUG
infoM = \x -> logM x INFO
warningM = \x -> logM x WARNING
errorM = \x -> logM x ERROR