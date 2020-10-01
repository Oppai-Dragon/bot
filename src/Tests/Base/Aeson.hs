module Tests.Base.Aeson
  ( baseAesonTests
  ) where

import Base.Aeson
import Config

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Test.HUnit

baseAesonTests :: [Test]
baseAesonTests =
  [ TestLabel "getValueTest" getValueTest
  , TestLabel "insertStringWithPushTest" insertStringWithPushTest
  , TestLabel "insertArrayWithPushTest" insertArrayWithPushTest
  , TestLabel "insertObjectWithPushTest" insertObjectWithPushTest
  , TestLabel "insertNumberWithPushTest" insertNumberWithPushTest
  , TestLabel "findValueTest" findValueTest
  ]

getValueTest, insertStringWithPushTest, insertArrayWithPushTest, insertObjectWithPushTest, insertNumberWithPushTest, findValueTest ::
     Test
getValueTest =
  TestCase $
  assertEqual "for (getValue [\"repeatN\"] testConfig)" (A.Number 1) $
  getValue ["repeatN"] testConfig

insertStringWithPushTest =
  TestCase $
  assertEqual
    "for (insertWithPush \"attachment\" (A.String \"photo1_2_3\") (HM.singleton \"attachment\" $ A.String \"photo2_3_4\"))"
    (HM.singleton "attachment" $ A.String "photo2_3_4,photo1_2_3") $
  insertWithPush
    "attachment"
    (A.String "photo1_2_3")
    (HM.singleton "attachment" $ A.String "photo2_3_4")

insertArrayWithPushTest =
  TestCase $
  assertEqual
    "for (insertWithPush \"params\" (A.Array $ V.singleton \"p2\") (HM.singleton \"params\" . A.Array $ V.singleton \"p1\"))"
    (HM.singleton "params" . A.Array $ V.fromList ["p1", "p2"]) $
  insertWithPush
    "params"
    (A.Array $ V.singleton "p2")
    (HM.singleton "params" . A.Array $ V.singleton "p1")

insertObjectWithPushTest =
  TestCase $
  assertEqual
    "for (insertWithPush \"methods\" (HM.singleton \"query\" $ A.Null) (HM.singleton \"methods\" $ A.object []))"
    (HM.singleton "methods" $ A.object ["query" A..= A.Null]) $
  insertWithPush
    "methods"
    (A.object ["query" A..= A.Null])
    (HM.singleton "methods" $ A.object [])

insertNumberWithPushTest =
  TestCase $
  assertEqual
    "for (insertWithPush \"id\" (A.Number 1) (HM.singleton \"id\" $ A.Number 1))"
    (HM.singleton "id" $ A.Number 2) $
  insertWithPush "id" (A.Number 1) (HM.singleton "id" $ A.Number 1)

findValueTest =
  TestCase $
  assertEqual
    "for (findValue [\"k1\",\"k2\"] (HM.fromList [(\"k1\",A.Null),(\"k2\",A.object [\"kek\" A..= A.Null])]))"
    (Just ("k2", A.object ["kek" A..= A.Null])) $
  findValue
    ["k1", "k2"]
    (HM.fromList [("k1", A.Null), ("k2", A.object ["kek" A..= A.Null])])
