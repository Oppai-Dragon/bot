module Base.Aeson
  ( valueToInteger
  , fromObj
  , fromString
  , deleteKeys
  , getValue
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Data.Maybe

type Keys = [T.Text]

valueToInteger :: A.Value -> Integer
valueToInteger = fromMaybe 0 . AT.parseMaybe A.parseJSON

fromString :: A.Value -> T.Text
fromString = fromMaybe "" . AT.parseMaybe A.parseJSON

fromObj :: A.Value -> A.Object
fromObj = fromMaybe HM.empty . AT.parseMaybe A.parseJSON

deleteKeys :: Keys -> A.Object -> A.Object
deleteKeys = foldr ((.) . HM.delete) id

getValue :: [T.Text] -> A.Object -> A.Value
getValue [] obj = A.Object obj
getValue (field:rest) objOld =
  case AT.parseMaybe (A..: field) objOld of
    Just (A.Object objNew) -> getValue rest objNew
    Just value -> value
    Nothing -> A.Null
