module AnimalClub.Animals.Examples.Goat (
  goatAnimalNode
  , goatPropertyList

  -- for feeding goats
  , worm
  , worm2
) where

import           AnimalClub.Animals
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Linear
import           AnimalClub.Skellygen.TRS


-- | whatever helper

relV3 :: (AnimalFloat a) => a -> a -> a -> AbsOrRel (V3 a)
relV3 x y z = Rel $ V3 x y z

{- goat
        ↑
        Y
      (_(
      /_/'_____/)
  ← X    "  |      |    →       (Z going into screen, RH coordinate system)
       |""""""|

        ↓
-}

-- | goat parameteris
goatBodyLegWidth :: (AnimalFloat a) => a
goatBodyLegWidth = 0.1
goatBodyLegVert :: (AnimalFloat a) => a
goatBodyLegVert = -0.01
goatLeg :: (AnimalFloat a) => a
goatLeg = 0.6

-- | front leg!
goatFrontRightLeg :: (AnimalFloat a) => AnimalNode a
goatFrontRightLeg = asPhantom $
  manf "leg" [BF_Right, BF_Front] (relV3 (0.05) goatBodyLegVert goatBodyLegWidth) (Rel 0.3)
    [manf "knee" [BF_Right, BF_Front] (relV3 (-0.05) (-goatLeg/2.0) 0) (Rel 0.8)
      [manf "ankle" [BF_Right, BF_Front] (relV3 (0.05) (-goatLeg/2.0) 0) (Rel 0.8)
        [manf "toe" [BF_Right, BF_Front] (relV3 (-0.1) 0 0) (Rel 0.4) []]]]

-- | back leg!
goatBackRightLeg :: (AnimalFloat a) => AnimalNode a
goatBackRightLeg = asPhantom $
  manf "leg" [BF_Right, BF_Back] (relV3 (-0.05) goatBodyLegVert goatBodyLegWidth) (Rel 0.38)
    [manf "knee" [BF_Right, BF_Back] (relV3 (-0.05) (-goatLeg/2.0) 0) (Rel 0.8)
      [manf "ankle" [BF_Right, BF_Back] (relV3 (0.05) (-goatLeg/2.0) 0) (Rel 0.8)
        [manf "toe" [BF_Right, BF_Back] (relV3 (-0.1) 0 0) (Rel 0.4) []]]]

-- | goat!
goatAnimalNode :: AnimalNode Float
goatAnimalNode = asPhantom $
  mans "root" (relV3 0 0 0) (Abs 0.2)
    [mans "neck" (relV3 (-0.07) (0.3) 0) (Rel 0.75)
      [mans "head" (relV3 (-0.07) (0.04) 0) (Rel 1.2) []]
    ,goatFrontRightLeg
    ,(flipAnimalNode ReflZ (defTransFlag ReflZ) goatFrontRightLeg)
    ,mans "body" (relV3 (0.35) (-0.02) 0) (Rel 0.95)
      [mans "body2" (relV3 (0.35) (0.02) 0) (Rel 1.05)
        [asPhantom $ mans "tailbone" (relV3 0 (0.2) 0) (Rel 0.2)
          [mans "tailend" (relV3 (0.1) (-0.02) 0) (Rel 1.0) []]
        ,goatBackRightLeg
        ,(flipAnimalNode ReflZ (defTransFlag ReflZ) goatBackRightLeg)
        ]
      ]
    ]

gdOrient = (-0.5, 0.5) :: (Float, Float)
gdLength = (0, 2) :: (Float, Float)
gdThick = (0, 2) :: (Float, Float)

-- NOTE, this is setting thickness on some phantom nodes which is pointless and harmless
goatPropertyList :: [(SkellyFunc Float, AutoGeneMethod Float)]
goatPropertyList =
 sfAutoGenome (WithBoneMatcher (nameFlagMatcher "leg" [BF_Front])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "knee" [BF_Front])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "ankle" [BF_Front])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "toe" [BF_Front])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "leg" [BF_Back])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "knee" [BF_Back])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "ankle" [BF_Back])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "toe" [BF_Back])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneId (BoneId "neck" [])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneId (BoneId "head" [])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneId (BoneId "body" [])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneId (BoneId "body2" [])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneId (BoneId "tailbone" [])) (gdLength,gdThick,gdOrient)
 ++ sfAutoGenome (WithBoneId (BoneId "tailend" [])) (gdLength,gdThick,gdOrient)


-- | your basic worm
worm :: AnimalNode Float
worm = asPhantom $
  mans "0" (relV3 0 0 0) (Abs 0.1)
    [mans "1" (relV3 1 0 0) (Rel 1)
      [mans "2" (relV3 0 1 0) (Rel 1)
        [mans "3" (relV3 1 0 0) (Rel 1)
          [mans "4" (relV3 0 1 0) (Rel 1) []]]]]

-- | a worm intended to be flipped that scales geometrically
flipWorm =
  mans "1" (relV3 1 0 0) (Rel 1.1)
    [mans "2" (relV3 0 0 1) (Rel 1.1)
      [mans "3" (relV3 1 0 0) (Rel 1.1)
        [mans "3" (relV3 0 0 1) (Rel 1.1) []]]]

-- | worm made from flipping a flippable worm
worm2 :: AnimalNode Float
worm2 = asPhantom $ mans "root" (relV3 0 0 0) (Abs 0.2)
  [flipWorm
  , flipAnimalNode ReflX (defTransFlag ReflX) flipWorm
  ]
