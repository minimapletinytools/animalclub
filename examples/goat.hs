{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Animals
import AnimalClub.Animal.Animal
import AnimalClub.Genetics
import AnimalClub.Skellygen
import AnimalClub.Skellygen.Math.Hierarchical
import AnimalClub.Skellygen.Math.Mesh
import           Linear.V3
import Data.Text as T
import Text.Printf (printf)
import System.Random
import Control.DeepSeq

import Debug.Trace

--AllBones'

sfGoatFront name = SkellyFunc (EnumBones' name [0,1])
sfGoatBack name = SkellyFunc (EnumBones' name [2,3])


defOrient = (-0.2, 0.2)
defLength = (-0.2,0.2)
defThick = (-1, 1)


-- Goat properties
goatPropertyList :: [(SkellyFunc, AutoGeneMethod)]
goatPropertyList = [
    (sfGoatFront "leg" Thickness, Normal defThick 1)
    , (sfGoatFront "leg" Orientation, Normal defOrient 3)
    --, (sfGoatFront "leg" Length, Normal defLength 1)
    , (sfGoatFront "knee" Thickness, Normal defThick 1)
    , (sfGoatFront "knee" Orientation, Normal defOrient 3)
    , (sfGoatFront "knee" Length, Normal defLength 1)
    , (sfGoatFront "ankle" Thickness, Normal defThick 1)
    , (sfGoatFront "ankle" Orientation, Normal defOrient 3)
    , (sfGoatFront "ankle" Length, Normal defLength 1)
    , (sfGoatFront "toe" Thickness, Normal defThick 1)
    , (sfGoatFront "toe" Length, Normal defLength 1)
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
    putStrLn $ (evalGenome goatGenome original) `deepseq` "genome evaled"
    writeFile "wigglygoat.obj" . meshToObj . generateMesh $ skelly
