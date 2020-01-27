{-

this example uses AnimalClub.Animals.Builder to make a goat

UNFINISH/WIP

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import           AnimalClub.Animals.Examples

import           AnimalClub.Animals
import           AnimalClub.Genetics
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Mesh
import           AnimalClub.Skellygen.TRS

import           System.Random
import           Text.Printf                 (printf)


main :: IO ()
main = do
    printf "Breeding goats...\n" :: IO ()
    gen <- getStdGen
    let
        goatDNALength = 1000
        goatGenome = makeGenomeFromPropertiesSimple goatDNALength [] goatPropertyList
        original = makeRandDNA gen goatDNALength
        goatProps = generateAnimalProperties (makeBoneIdList goat) $ evalGenome goatGenome original
        skelly = animalNodeToSkellyNodeWithProps goatProps goat
    writeFile "wigglygoat.obj" . meshToObj . generateMesh $ skelly
