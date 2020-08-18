{-# LANGUAGE LambdaCase #-}

module Config.Set
  ( set
  , setConfig
  ) where

import Base

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

set :: FilePath -> IO Object
set path =
  BSL.readFile path >>= pure . decode >>= \case
    Just hm -> pure hm
    Nothing -> pure HM.empty

setConfig :: IO Object
setConfig = do
  repDir <- getRepDir
  config <- set $ repDir <> "\\src\\Config.json"
  let bot =
        case parseMaybe (.: "bot") config of
          Just (String name) -> name
          _ -> ""
  let botPath = T.unpack $ "Bot\\" <> bot <> "\\" <> bot <> ".json"
  botConfig <- set $ repDir <> botPath
  return $ HM.unions [botConfig, config]
