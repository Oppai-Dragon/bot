module Base.Aeson
  ( valueToInteger
  , fromString
  , fromObject
  , fromArrString
  , deleteKeys
  , findObject
  , getValue
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Data.Maybe

type Field = T.Text

type Keys = [Field]

valueToInteger :: A.Value -> Integer
valueToInteger = fromMaybe 0 . AT.parseMaybe A.parseJSON

fromString :: A.Value -> Field
fromString = fromMaybe "" . AT.parseMaybe A.parseJSON

fromObject :: A.Value -> A.Object
fromObject = fromMaybe HM.empty . AT.parseMaybe A.parseJSON

fromArrString :: A.Value -> [Field]
fromArrString = fromMaybe [] . AT.parseMaybe A.parseJSON

deleteKeys :: Keys -> A.Object -> A.Object
deleteKeys = foldr ((.) . HM.delete) id

findObject :: Keys -> A.Object -> Maybe (Field, A.Object)
findObject [] _ = Nothing
findObject (keyX:rest) obj =
  case getValue [keyX] obj of
    A.Object x -> Just (keyX, x)
    _ -> findObject rest obj

getValue :: Keys -> A.Object -> A.Value
getValue [] obj = A.Object obj
getValue (field:rest) objOld =
  case AT.parseMaybe (A..: field) objOld of
    Just (A.Object objNew) -> getValue rest objNew
    Just value -> value
    Nothing -> A.Null
