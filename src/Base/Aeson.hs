module Base.Aeson
  ( isArray
  , toInteger
  , toBS
  , toText
  , fromString
  , fromNumber
  , fromObject
  , fromArr
  , fromArrString
  , fromArrObject
  , deleteKeys
  , insertWithPush
  , findObject
  , getValue
  ) where

import Prelude hiding (toInteger)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

type Field = T.Text

type Keys = [Field]

isArray :: A.Value -> Bool
isArray (A.Array _) = True
isArray _ = False

toInteger :: A.Value -> Integer
toInteger = fromMaybe 0 . AT.parseMaybe A.parseJSON

toBS :: A.Value -> BS.ByteString
toBS value =
  case value of
    A.String text -> TE.encodeUtf8 text
    A.Number _ -> TE.encodeUtf8 . T.pack . show $ toInteger value
    A.Bool bool -> TE.encodeUtf8 . T.pack $ show bool
    _ -> ""

toText :: A.Value -> T.Text
toText value =
  case value of
    A.String text -> text
    A.Number _ -> T.pack . show $ toInteger value
    A.Bool bool -> T.pack $ show bool
    _ -> ""

fromString :: A.Value -> Field
fromString = fromMaybe "" . AT.parseMaybe A.parseJSON

fromNumber :: A.Value -> S.Scientific
fromNumber = fromMaybe 0 . AT.parseMaybe A.parseJSON

fromObject :: A.Value -> A.Object
fromObject = fromMaybe HM.empty . AT.parseMaybe A.parseJSON

fromArr :: A.Value -> [A.Value]
fromArr = fromMaybe [] . AT.parseMaybe A.parseJSON

fromArrString :: A.Value -> [Field]
fromArrString = fromMaybe [] . AT.parseMaybe A.parseJSON

fromArrObject :: A.Value -> [A.Object]
fromArrObject = fromMaybe [] . AT.parseMaybe A.parseJSON

deleteKeys :: Keys -> A.Object -> A.Object
deleteKeys = foldr ((.) . HM.delete) id

insertWithPush :: Field -> A.Value -> A.Object -> A.Object
insertWithPush field value obj =
  let pushFunc new oldValue =
        case oldValue of
          A.String old -> A.String $ old <> "," <> fromString new
          A.Array old -> A.Array . V.fromList $ V.toList old <> fromArr new
          A.Object old -> A.Object $ old `HM.union` fromObject new
          A.Number old -> A.Number $ old + fromNumber new
          _ -> A.Null
   in HM.insertWith pushFunc field value obj

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
