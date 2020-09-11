module Tests.Config
  ( configTests
  ) where

import Base.Interface
import Config

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

import Test.HUnit

configTests :: [Test]
configTests = [TestLabel "modifyConfigTest" modifyConfigTest]

modifyConfigTest :: Test
modifyConfigTest =
  TestCase $
  execApp
    (modifyConfig . HM.insert "key" $ A.String "value")
    (Handle HM.empty (error "empty log handle")) >>= \(Handle config _) ->
    assertEqual
      "for (execApp (modifyConfig . HM.insert \"key\" $ A.String \"value\") (Handle HM.empty (error \"empty log handle\")))"
      (HM.singleton "key" $ A.String "value")
      config
