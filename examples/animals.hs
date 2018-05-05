{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}


import AnimalClub.Skellygen
import AnimalClub.Skellygen.Math.Hierarchical
import AnimalClub.Skellygen.Math.Mesh
import           Linear.V3


relV3 x y z = Rel $ V3 x y z

worm =
    AnimalNode (Bone "0") (relV3 0 0 0) (Abs 0.1) True
        [AnimalNode (Bone "1") (relV3 1 0 0) (Rel 1) False
            [AnimalNode (Bone "2") (relV3 0 1 0) (Rel 1) False
                [AnimalNode (Bone "3") (relV3 1 0 0) (Rel 1) False
                    [AnimalNode (Bone "4") (relV3 0 1 0) (Rel 1) False []]]]]

flipWorm =
    AnimalNode (EnumBone "1" 0 Same) (relV3 1 0 0) (Rel 1.1) False
        [AnimalNode (EnumBone "2" 0 Same) (relV3 0 0 1) (Rel 1.1) False
            [AnimalNode (EnumBone "3" 0 Same) (relV3 1 0 0) (Rel 1.1) False
                [AnimalNode (EnumBone "3" 0 Same) (relV3 0 0 1) (Rel 1.1) False []]]]

worm2 = AnimalNode (Bone "root") (relV3 0 0 0) (Abs 0.2) True
    [flipWorm
    , flipAnimalNode ReflX 2 flipWorm
    ]

{- goat
                ↑
                Y
            (_(
            /_/'_____/)
     ← X    "  |      |    →       (Z out of screen, LH coordinate system, same as Unity. Ugg)
               |""""""|

                ↓
-}

goatBodyLegWidth = 0.1
goatBodyLegVert = -0.01
goatLeg = 0.6

goatFrontLeftLeg =
    AnimalNode (EnumBone "leg" 0 Same) (relV3 (0.05) goatBodyLegVert goatBodyLegWidth) (Rel 0.3) True
        [AnimalNode (EnumBone "knee" 0 Same) (relV3 (-0.05) (-goatLeg/2.0) 0) (Rel 0.8) False
            [AnimalNode (EnumBone "ankle" 0 Same) (relV3 (0.05) (-goatLeg/2.0) 0) (Rel 0.8) False
                [AnimalNode (EnumBone "toe" 0 Same) (relV3 (-0.1) 0 0) (Rel 0.4) False []]]]

goatBackLeftLeg =
    AnimalNode (EnumBone "leg" 2 Same) (relV3 (-0.05) goatBodyLegVert goatBodyLegWidth) (Rel 0.38) True
        [AnimalNode (EnumBone "knee" 2 Same) (relV3 (-0.05) (-goatLeg/2.0) 0) (Rel 0.8) False
            [AnimalNode (EnumBone "ankle" 2 Same) (relV3 (0.05) (-goatLeg/2.0) 0) (Rel 0.8) False
                [AnimalNode (EnumBone "toe" 2 Same) (relV3 (-0.1) 0 0) (Rel 0.4) False []]]]


-- | goat!
goat =
    AnimalNode (Bone "root") (relV3 0 0 0) (Abs 0.2) True
        [AnimalNode (Bone "neck") (relV3 (-0.07) (0.3) 0) (Rel 0.75) False
            [AnimalNode (Bone "head") (relV3 (-0.07) (0.04) 0) (Rel 1.2) False []]
        ,goatFrontLeftLeg
        ,(flipAnimalNode ReflZ 1 goatFrontLeftLeg)
        ,AnimalNode (Bone "body") (relV3 (0.35) (-0.02) 0) (Rel 0.95) False
            [AnimalNode (Bone "body2") (relV3 (0.35) (0.02) 0) (Rel 1.05) False
                [AnimalNode (Bone "tailbone") (relV3 0 (0.2) 0) (Rel 0.2) True
                    [AnimalNode (Bone "tailend") (relV3 (0.1) (-0.02) 0) (Rel 1.0) False []]
                ,goatBackLeftLeg
                ,(flipAnimalNode ReflZ 3 goatBackLeftLeg)
                ]
            ]
        ]

main = do
    writeFile "goat.obj" . meshToObj . generateMesh . animalNodeToSkellyNode $ goat
    writeFile "worm.obj" . meshToObj . generateMesh . animalNodeToSkellyNode $ worm2
