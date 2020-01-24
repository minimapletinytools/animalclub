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
import           AnimalClub.Skellygen.Math.Mesh
import           Linear.V3

-- | whatever helper
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
goatBodyLegWidth = 0.1
goatBodyLegVert = -0.01
goatLeg = 0.6

-- | front leg!
goatFrontRightLeg = asRoot $
    manf "leg" [BF_Right, BF_Front] (relV3 (0.05) goatBodyLegVert goatBodyLegWidth) (Rel 0.3)
        [manf "knee" [BF_Right, BF_Front] (relV3 (-0.05) (-goatLeg/2.0) 0) (Rel 0.8)
            [manf "ankle" [BF_Right, BF_Front] (relV3 (0.05) (-goatLeg/2.0) 0) (Rel 0.8)
                [manf "toe" [BF_Right, BF_Front] (relV3 (-0.1) 0 0) (Rel 0.4) []]]]

-- | back leg!
goatBackRightLeg = asRoot $
    manf "leg" [BF_Right, BF_Back] (relV3 (-0.05) goatBodyLegVert goatBodyLegWidth) (Rel 0.38)
        [manf "knee" [BF_Right, BF_Back] (relV3 (-0.05) (-goatLeg/2.0) 0) (Rel 0.8)
            [manf "ankle" [BF_Right, BF_Back] (relV3 (0.05) (-goatLeg/2.0) 0) (Rel 0.8)
                [manf "toe" [BF_Right, BF_Back] (relV3 (-0.1) 0 0) (Rel 0.4) []]]]

-- | goat!
goat = asRoot $
    mans "root" (relV3 0 0 0) (Abs 0.2)
        [mans "neck" (relV3 (-0.07) (0.3) 0) (Rel 0.75)
            [mans "head" (relV3 (-0.07) (0.04) 0) (Rel 1.2) []]
        ,goatFrontRightLeg
        ,(flipAnimalNode ReflZ (defTransFlag ReflZ) goatFrontRightLeg)
        ,mans "body" (relV3 (0.35) (-0.02) 0) (Rel 0.95)
            [mans "body2" (relV3 (0.35) (0.02) 0) (Rel 1.05)
                [asRoot $ mans "tailbone" (relV3 0 (0.2) 0) (Rel 0.2)
                    [mans "tailend" (relV3 (0.1) (-0.02) 0) (Rel 1.0) []]
                ,goatBackRightLeg
                ,(flipAnimalNode ReflZ (defTransFlag ReflZ) goatBackRightLeg)
                ]
            ]
        ]

-- | your basic worm
worm = asRoot $
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

-- | worm made from flipping a flippable worm
worm2 = asRoot $ mans "root" (relV3 0 0 0) (Abs 0.2)
    [flipWorm
    , flipAnimalNode ReflX (defTransFlag ReflX) flipWorm
    ]

-- | write the example skeletons
writeExampleObjs = do
    writeFile "goat.obj" . meshToObj . generateMesh . animalNodeToSkellyNode $ goat
    writeFile "worm.obj" . meshToObj . generateMesh . animalNodeToSkellyNode $ worm
    writeFile "worm2.obj" . meshToObj . generateMesh . animalNodeToSkellyNode $ worm2
