module Log.Msg where

import Log
import Log.Methods

import Config
import Config.Get

import Data.Aeson
import Data.Char (isDigit)
import qualified Data.Text as T

import System.IO.Unsafe (unsafePerformIO)

config = unsafePerformIO setConfig

getLevel :: Priority
getLevel = case getValue ["logLevel"] config of
    String text -> read $ T.unpack text
    _           -> DEBUG

whichLog :: Priority -> IO () -> IO ()
whichLog level print' =
    if level >= getLevel
        then print'
        else defaultLog

defaultLog = putStr ""
parseAttachment = takeWhile (not . isDigit)

--bot = getConfig setConfig "whichBot"

--logWhichBot = infoM "Now bot is " bot

logKeySession conf json = undefined
--    let
--        bot = getValue ["bot"] conf
--        logPare = logMsg "keySession"
--        server = getValue ["server"] conf
--        key = getValue ["key"] conf
--        ts = getValue ["ts"] conf
--        params = if bot == "BotVk" then [server,key,ts] else ["Just another"]
--    in if any ("NOTHING"==) params
--        then whichLog ERROR (errorM "" "Failed askRequest")
--            >> putStr "You got " >> print json
--        else whichLog (fst logPare) $ logM "" (fst logPare) (snd logPare)