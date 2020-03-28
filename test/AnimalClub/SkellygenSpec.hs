{-# OPTIONS_GHC -fno-warn-orphans #-}

module AnimalClub.SkellygenSpec (
  spec
) where

import           Test.Hspec
import           Test.HUnit
import           Test.QuickCheck

import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Linear hiding (trace)
import           AnimalClub.Skellygen.TRS

import           Linear.Arbitrary

--import           Debug.Trace

prop_justfortesting :: Int -> Bool
prop_justfortesting x = (read . show) x == x

-- TODO finish
{-
instance (Arbitrary a) => Arbitrary (AnimalNode a) where
  arbitrary = sized arb where
    arb 0 = do
      pos <- arbitrary
      return AnimalNode {
          _name = undefined
          , _pos = Abs pos
          , _thickness = undefined
          , _isPhantom = True
          , _children = []
        }
-}

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
instance (Arbitrary a, AnimalFloat a) => Arbitrary (TRS a) where
  arbitrary = do
    t <- arbitrary
    r <- arbitrary
    NonZeroV3 s <- arbitrary
    return $ TRS t r s

prop_mul_TRS_V3 :: TRS Double -> V3 Double -> Bool
prop_mul_TRS_V3 trs v = pass where
  mul_TRS_V3' (TRS pt pr ps) ct = pt ^+^ (pr `rotate` (conv_Scale_M33 ps !* ct))
  p = mul_TRS_V3 trs v
  p' = mul_TRS_V3' trs v
  pass = nearZero (p-p')

prop_fromTo :: V3 Double -> V3 Double -> Bool
prop_fromTo v1 v2 = pass where
  r = fromTo v1 v2
  v2' = r `rotate` v1
  pass = nearZero (v2 `cross` v2')

spec_fromEulerXYZ :: Spec
spec_fromEulerXYZ = do
  it "handles identity properly" $
    assertBool "" $ nearZero (fromEulerXYZ (V3 0 0 0 :: V3 Double) - identityRotation)
  it "handles 90 deg around X properly" $
    assertBool "" $ nearZero (fromEulerXYZ (V3 pio2 0 0) - Quaternion r2o2 (V3 r2o2 0 0)) where
      pio2 = 1.57079632679  :: Double
      r2o2 = 0.70710678118
  -- TODO more useful test cases

spec :: Spec
spec = do
  describe "quickcheck" $ do
    it "does in fact work with spec" $ property $
      prop_justfortesting
  describe "TRS" $ do
    it "mul_TRS_V3 works as expected" $ property $
      prop_mul_TRS_V3
    it "fromTo works as expected" $ property $
      prop_fromTo
    describe "fromEulerXYZ" $
      spec_fromEulerXYZ
