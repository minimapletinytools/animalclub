{-

this example uses AnimalClub.Animals.Builder to make a goat

UNFINISH/WIP

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import           ExamplesLib.Skeletons

import           AnimalClub.Animals
import           AnimalClub.Genetics
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Math.Mesh

import           System.Random
import           Text.Printf                    (printf)



-- data DepFunc = Linear Float | Mutate Float

-- data ATree = ATree ((Either T.Text SkellyFunc, AutoGeneMethod), [(DepFunc, ATree)])

-- energyNode = ATree ((Left "energy consumption", Normal (0,2) 10), [(Linear 1, undefined)])




sfGoatFront name = SkellyFunc (EnumBones' name [0,1])
sfGoatBack name = SkellyFunc (EnumBones' name [2,3])

gdOrient = (-0.5, 0.5)
gdLength = (0, 2)
gdThick = (0, 2)
--gdOrient = (0,0)
--gdLength = (0,0)
--gdThick = (0,0)

-- Goat properties
goatPropertyList :: [(SkellyFunc, AutoGeneMethod)]
goatPropertyList = [
    (sfGoatFront "leg" defThickness, Normal gdThick 1)
    , (sfGoatFront "leg" defOrientation, Normal gdOrient 3)
    --, (sfGoatFront "leg" defLength, Normal gdLength 1)
    , (sfGoatFront "knee" defThickness, Normal gdThick 1)
    , (sfGoatFront "knee" defOrientation, Normal gdOrient 3)
    , (sfGoatFront "knee" defLength, Normal gdLength 1)
    , (sfGoatFront "ankle" defThickness, Normal gdThick 1)
    , (sfGoatFront "ankle" defOrientation, Normal gdOrient 3)
    , (sfGoatFront "ankle" defLength, Normal gdLength 1)
    , (sfGoatFront "toe" defThickness, Normal gdThick 1)
    , (sfGoatFront "toe" defLength, Normal gdLength 1)

    , (sfGoatBack "leg" defThickness, Normal gdThick 1)
    , (sfGoatBack "leg" defOrientation, Normal gdOrient 3)
    --, (sfGoatBack "leg" defLength, Normal gdLength 1)
    , (sfGoatBack "knee" defThickness, Normal gdThick 1)
    , (sfGoatBack "knee" defOrientation, Normal gdOrient 3)
    , (sfGoatBack "knee" defLength, Normal gdLength 1)
    , (sfGoatBack "ankle" defThickness, Normal gdThick 1)
    , (sfGoatBack "ankle" defOrientation, Normal gdOrient 3)
    , (sfGoatBack "ankle" defLength, Normal gdLength 1)
    , (sfGoatBack "toe" defThickness, Normal gdThick 1)
    , (sfGoatBack "toe" defLength, Normal gdLength 1)

    , (SkellyFunc (Bone' "neck") defThickness, Normal gdThick 1)
    , (SkellyFunc (Bone' "neck") defOrientation, Normal gdOrient 3)
    , (SkellyFunc (Bone' "neck") defLength, Normal gdLength 1)

    , (SkellyFunc (Bone' "head") defThickness, Normal gdThick 1)
    , (SkellyFunc (Bone' "head") defOrientation, Normal gdOrient 3)
    , (SkellyFunc (Bone' "head") defLength, Normal gdLength 1)

    , (SkellyFunc (Bone' "body") defThickness, Normal gdThick 1)
    , (SkellyFunc (Bone' "body") defOrientation, Normal gdOrient 3)
    , (SkellyFunc (Bone' "body") defLength, Normal gdLength 1)

    , (SkellyFunc (Bone' "body2") defThickness, Normal gdThick 1)
    , (SkellyFunc (Bone' "body2") defOrientation, Normal gdOrient 3)
    , (SkellyFunc (Bone' "body2") defLength, Normal gdLength 1)

    , (SkellyFunc (Bone' "tailbone") defThickness, Normal gdThick 1)
    , (SkellyFunc (Bone' "tailbone") defOrientation, Normal gdOrient 3)
    , (SkellyFunc (Bone' "tailbone") defLength, Normal gdLength 1)

    , (SkellyFunc (Bone' "tailend") defThickness, Normal gdThick 1)
    , (SkellyFunc (Bone' "tailend") defThickness, Normal gdOrient 3)
    , (SkellyFunc (Bone' "tailend") defLength, Normal gdLength 1)

    ]

main :: IO ()
main = do

    printf "Breeding goats...\n" :: IO ()
    gen <- getStdGen
    let
        goatDNALength = 1000
        goatGenome = makeGenomeFromPropertiesSimple goatDNALength [] goatPropertyList
        original = makeRandDNA gen goatDNALength
        goatProps = generateAnimalProperties $ evalGenome goatGenome original
        skelly = animalNodeToSkellyNodeWithProps goatProps goat
    writeFile "wigglygoat.obj" . meshToObj . generateMesh $ skelly
