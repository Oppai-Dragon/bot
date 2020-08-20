{-# LANGUAGE LambdaCase #-}

module Config.Set
  ( set
  , setConfig
  ) where

import Base

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

set :: FilePath -> IO A.Object
set path =
  BSL.readFile path >>= pure . A.decode >>= \case
    Just hm -> pure hm
    Nothing -> pure HM.empty

setConfig :: IO A.Object
setConfig = do
  repDir <- getRepDir
  config <- set $ repDir <> "\\src\\Config.json"
  let bot =
        case AT.parseMaybe (A..: "bot") config of
          Just (A.String name) -> name
          _ -> ""
  let botPath = T.unpack $ "\\src\\Bot\\" <> bot <> "\\" <> bot <> ".json"
  botConfig <- set $ repDir <> botPath
  return $ HM.unions [botConfig, config]
