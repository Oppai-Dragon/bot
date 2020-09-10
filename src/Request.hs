module Request
  ( startRequest
  , askRequest
  , sendRequest
  ) where

import Base
import Config
import Config.Get
import Request.Exception
import Request.Modify

import Control.Monad
import qualified Data.Aeson as A
import qualified Network.HTTP.Simple as HTTPSimple

startRequest :: App A.Value
startRequest = do
  (Config.Handle config _) <- getApp
  let request = getStartRequest config
  tryHttpJson $ HTTPSimple.httpJSON request

askRequest :: App A.Value
askRequest = do
  (Config.Handle config _) <- getApp
  let request = getAskRequest config
  tryHttpJson $ HTTPSimple.httpJSON request

sendRequest :: App ()
sendRequest = do
  (Config.Handle config _) <- getApp
  let reqDefault = getSendRequest config
  request <- evalApp modifyRequest reqDefault
  void . tryHttpJson $ HTTPSimple.httpJSON request
