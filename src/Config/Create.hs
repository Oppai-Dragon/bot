module Config.Create
  ( maybeTCreate
  , maybeTCreateConfig
  ) where

import Base (liftIO)
import Bot
import Log.Console
import Log.Handle

import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import System.IO (stderr)

maybeTCreate :: FilePath -> MaybeT IO A.Object
maybeTCreate path = do
  result <- liftIO . try $ BSL.readFile path
  case result of
    Right bsl -> MaybeT . return $ A.decode bsl
    Left err -> do
      liftIO . logError defaultHandle {hLogFileHandle = stderr} $ show err
      MaybeT $ return Nothing

maybeTCreateConfig :: MaybeT IO A.Object
maybeTCreateConfig = do
  config <- maybeTCreate "configs/Config.json"
  let maybeBot = AT.parseMaybe (\x -> x A..: "bot" >>= A.parseJSON) config
  case maybeBot of
    Just bot -> do
      let botStr = show (bot :: Bot)
      let botPath = "configs/" <> botStr <> "/" <> botStr <> ".json"
      botConfig <- maybeTCreate botPath
      return $ HM.union botConfig config
    Nothing -> do
      liftIO $
        logError
          defaultHandle {hLogFileHandle = stderr}
          "Can't find bot implementation"
      MaybeT $ return Nothing
