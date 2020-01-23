{-|
Module      : Animals
Description : Example animals skeletons
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

provides example AnimalClub.Skellygen skeletons

-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module ExamplesLib.Skeletons (
    goat,
    writeExampleObjs
) where

import           AnimalClub.Animals
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Math.Hierarchical
import           AnimalClub.Skellygen.Math.Mesh
import           Linear.V3

-- | whatever helper
relV3 x y z = Rel $ V3 x y z

{- goat
                ↑
                Y
            (_(
            /_/'_____/)
     ← X    "  |      |    →       (Z out of screen, LH coordinate system, same as Unity. Ugg)
               |""""""|

                ↓
-}

-- | goat parameteris
goatBodyLegWidth = 0.1
goatBodyLegVert = -0.01
goatLeg = 0.6

-- | front leg!
goatFrontLeftLeg = setRoot $
    manf "leg" [BF_Left, BF_Front] (relV3 (0.05) goatBodyLegVert goatBodyLegWidth) (Rel 0.3)
        [manf "knee" [BF_Left, BF_Front] (relV3 (-0.05) (-goatLeg/2.0) 0) (Rel 0.8)
            [manf "ankle" [BF_Left, BF_Front] (relV3 (0.05) (-goatLeg/2.0) 0) (Rel 0.8)
                [manf "toe" [BF_Left, BF_Front] (relV3 (-0.1) 0 0) (Rel 0.4) []]]]

-- | back leg!
goatBackLeftLeg = setRoot $
    manf "leg" [BF_Left, BF_Back] (relV3 (-0.05) goatBodyLegVert goatBodyLegWidth) (Rel 0.38)
        [manf "knee" [BF_Left, BF_Back] (relV3 (-0.05) (-goatLeg/2.0) 0) (Rel 0.8)
            [manf "ankle" [BF_Left, BF_Back] (relV3 (0.05) (-goatLeg/2.0) 0) (Rel 0.8)
                [manf "toe" [BF_Left, BF_Back] (relV3 (-0.1) 0 0) (Rel 0.4) []]]]

-- | goat!
goat = setRoot $
    mans "root" (relV3 0 0 0) (Abs 0.2)
        [mans "neck" (relV3 (-0.07) (0.3) 0) (Rel 0.75)
            [mans "head" (relV3 (-0.07) (0.04) 0) (Rel 1.2) []]
        ,goatFrontLeftLeg
        ,(flipAnimalNode ReflZ (defTransFlag ReflZ) goatFrontLeftLeg)
        ,mans "body" (relV3 (0.35) (-0.02) 0) (Rel 0.95)
            [mans "body2" (relV3 (0.35) (0.02) 0) (Rel 1.05)
                [setRoot $ mans "tailbone" (relV3 0 (0.2) 0) (Rel 0.2)
                    [mans "tailend" (relV3 (0.1) (-0.02) 0) (Rel 1.0) []]
                ,goatBackLeftLeg
                ,(flipAnimalNode ReflZ (defTransFlag ReflZ) goatBackLeftLeg)
                ]
            ]
        ]

-- | your basic worm
worm = setRoot $
    mans "0" (relV3 0 0 0) (Abs 0.1)
        [mans "1" (relV3 1 0 0) (Rel 1)
            [mans "2" (relV3 0 1 0) (Rel 1)
                [mans "3" (relV3 1 0 0) (Rel 1)
                    [mans "4" (relV3 0 1 0) (Rel 1) []]]]]

-- | flippable worm
flipWorm =
    mans "1" (relV3 1 0 0) (Rel 1.1)
        [mans "2" (relV3 0 0 1) (Rel 1.1)
            [mans "3" (relV3 1 0 0) (Rel 1.1)
                [mans "3" (relV3 0 0 1) (Rel 1.1) []]]]

-- TODO just flip `worm` and delete `flipWorm`
-- | worm made from flipping a flippable worm
worm2 = setRoot $ mans "root" (relV3 0 0 0) (Abs 0.2)
    [flipWorm
    , flipAnimalNode ReflX (defTransFlag ReflX) flipWorm
    ]

-- | write the example skeletons
writeExampleObjs = do
    writeFile "goat.obj" . meshToObj . generateMesh . animalNodeToSkellyNode $ goat
    writeFile "worm.obj" . meshToObj . generateMesh . animalNodeToSkellyNode $ worm
    writeFile "worm2.obj" . meshToObj . generateMesh . animalNodeToSkellyNode $ worm2
