--{-# OPTIONS_GHC -fno-warn-orphans #-}
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Math.Hierarchical
import           AnimalClub.Skellygen.Math.Quaternion   as Q
import           AnimalClub.Skellygen.Math.TRS
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Modifiers

import           Linear.Arbitrary
import           Linear.Epsilon
import qualified Linear.Matrix                          as M
import           Linear.Metric                          as Metric

import           Debug.Trace
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


-- math stuff

-- | this only generates "nice" TRS (nothing weird going on in scale component)
instance (Arbitrary a, Epsilon a, Floating a) => Arbitrary (TRS a) where
  arbitrary = do
    t <- arbitrary
    r <- arbitrary
    DiagM33 s <- arbitrary `suchThat` (not . nearZero . M.det33 . unDiagM33)
    return $ TRS t r s



prop_invTRS :: TRS Double -> Bool
prop_invTRS trs = pass where
  help@(TRS t r s) = (trs `inherit` invTRS trs)
  pass = trace (show help) $ nearZero t && nearZero (Metric.distance r Q.identity) && nearZero (s - M.identity)

main :: IO ()
main = hspec $ do
  describe "quickcheck" $ do
    it "does in fact work with spec" $ property $
      prop_justfortesting
  describe "TRS" $ do
    it "invTRS works as expected" $ property $
      prop_invTRS
