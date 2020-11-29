module Tests.Base
  ( baseTests
  ) where

import Tests.Base.Aeson
import Tests.Base.Prelude

import Test.HUnit

baseTests :: [Test]
baseTests = baseAesonTests <> basePreludeTests
