module LogMsg where

import LogConfig
import Log 
    ( Priority (..)
    , logM
    , debugM
    , infoM
    , warningM
    , errorM
    )
import Config 
    ( Config
    , setConfig
    , getConfig
    )
import Data.Char (isDigit)

getLevel :: Priority
getLevel = read $ getConfig setConfig "logLevel"

whichLog :: Priority -> IO () -> IO ()
whichLog level print' = 
    if level >= getLevel
        then print'
        else defaultLog

defaultLog = putStr ""
parseAttachment = takeWhile (not . isDigit) 

bot = getConfig setConfig "whichBot"

logWhichBot =
    infoM "Now bot is " bot

logKeySession conf json =
    let 
        logPare = logMsg "keySession"
        server = getConfig conf "server"
        key = getConfig conf "key"
        ts = getConfig conf "ts"
        params = if bot == "BotVk" then [server,key,ts] else ["Just another"]
    in if any ("NOTHING"==) params
        then whichLog ERROR (errorM "" "Failed askRequest")
            >> putStr "You got " >> print json
        else whichLog (fst logPare) $ logM "" (fst logPare) (snd logPare)