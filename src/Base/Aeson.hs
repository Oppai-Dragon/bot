module Base.Aeson
  ( isArray
  , valueToInteger
  , fromString
  , fromObject
  , fromArrString
  , fromArrObject
  , deleteKeys
  , insertWithPush
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

isArray :: A.Value -> Bool
isArray (A.Array _) = True
isArray _ = False

valueToInteger :: A.Value -> Integer
valueToInteger = fromMaybe 0 . AT.parseMaybe A.parseJSON

fromString :: A.Value -> Field
fromString = fromMaybe "" . AT.parseMaybe A.parseJSON

fromObject :: A.Value -> A.Object
fromObject = fromMaybe HM.empty . AT.parseMaybe A.parseJSON

fromArrString :: A.Value -> [Field]
fromArrString = fromMaybe [] . AT.parseMaybe A.parseJSON

fromArrObject :: A.Value -> [A.Object]
fromArrObject = fromMaybe [] . AT.parseMaybe A.parseJSON

deleteKeys :: Keys -> A.Object -> A.Object
deleteKeys = foldr ((.) . HM.delete) id

insertWithPush :: Field -> A.Value -> A.Object -> A.Object
insertWithPush =
  HM.insertWith (\new old -> A.String $ fromString old <> fromString new)

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
