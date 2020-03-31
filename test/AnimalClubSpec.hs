{-# OPTIONS_GHC -fno-warn-orphans #-}

module AnimalClubSpec (
  spec
) where

import           Test.Hspec
import           Test.HUnit

-- TODO test that symmetries are preserved
--spec_symmetry :: Spec
--spec_symmetry = do

spec :: Spec
spec = do
  describe "AnimalClub" $ do
    it "dummy test" $ True `shouldBe` True
