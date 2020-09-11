module Base.Time
  ( getTime
  ) where

import qualified Data.Time.LocalTime as LocalTime

getTime :: IO String
getTime = do
  zonedTime <- LocalTime.getZonedTime
  let zonedTimeStr = show zonedTime
  let time = takeWhile (/= '.') . tail $ dropWhile (/= ' ') zonedTimeStr
  return time
