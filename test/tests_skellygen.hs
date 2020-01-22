--{-# OPTIONS_GHC -fno-warn-orphans #-}
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Math.Hierarchical
import           Test.Hspec
import           Test.QuickCheck

import           Linear.Arbitrary

--prop_TRS

prop_justfortesting :: Int -> Bool
prop_justfortesting x = (read . show) x == x


-- TODO finish
instance Arbitrary AnimalNode where
  arbitrary = sized arb where
    arb 0 = do
      pos <- arbitrary
      return AnimalNode {
          _name = undefined
          , _pos = Abs pos
          , _thickness = undefined
          , _isRoot = True
          , _children = []
        }

-- TODO flips an animal node using ReflX
-- and checks that all X coordinates cancel out
--prop_flipAnimalNode_ReflX :: Bool

-- TODO spins an animal node around several times (total 360 degrees)
-- and checks that all coordinates around spun axis add to 0
--prop_flipAnimalNode_Spin :: Bool

-- TODO creates an animalNode using flipAnimalNode twice and checks that the nodes that got flipped twice match their unflipped values
--spec_flipAnimalNode_Twice :: Bool

main :: IO ()
main = hspec $ do
  describe "quickcheck" $ do
    it "does in fact work with spec" $ property $
      prop_justfortesting
