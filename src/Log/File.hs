module Log.File
  ( setLogPath
  , writeLog
  ) where

import qualified Data.Time.Clock as UTC
import qualified System.Directory as Dir
import qualified System.IO as IO

setLogPath :: IO FilePath
setLogPath = do
  let logsDir = "logs"
  Dir.createDirectoryIfMissing False logsDir
  (UTC.UTCTime day _) <- UTC.getCurrentTime
  let logPath = logsDir <> "/" <> show day <> ".txt"
  return logPath

writeLog :: FilePath -> String -> IO ()
writeLog = IO.appendFile
