{-# OPTIONS_GHC -fno-warn-orphans #-}

module AnimalClubSpec (
  spec
) where

import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = do
  describe "AnimalClub" $ do
    it "dummy test" $ True `shouldBe` True
