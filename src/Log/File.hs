module Log.File
  ( setLogPath
  ) where

import Base

import qualified Data.Time.Clock as UTC
import qualified System.Directory as Dir

setLogPath :: IO FilePath
setLogPath = do
  repPath <- getRepDir
  let logsDirPath = repPath <> "\\logs"
  Dir.createDirectoryIfMissing False logsDirPath
  (UTC.UTCTime day time) <- UTC.getCurrentTime
  let logFile = "Log." <> show day <> ".txt"
  let logPath = logsDirPath <> "\\" <> logFile
  return logPath
