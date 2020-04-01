{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import           Relude

import           AnimalClub.Animals.Examples

import           AnimalClub.Animals
import           AnimalClub.Genetics
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Mesh

import qualified Data.Text.IO                as T
import           System.Random
import           Text.Printf                 (printf)


main :: IO ()
main = do
  writeExampleObjs
  printf "Breeding goats...\n" :: IO ()
  gen <- getStdGen
  let
    goatDNALength = 1000
    goatGenome = makeGenomeFromPropertiesSimple goatDNALength [] goatPropertyList
    original = makeRandDNA gen goatDNALength
    goatProps = generateAnimalProperties (makeBoneIdList goatAnimalNode) $ evalGenome goatGenome original
    skelly = animalNodeToSkellyNodeWithProps goatProps goatAnimalNode
  T.writeFile "wigglygoat.obj" . meshToObj . generateLocalMesh $ skelly
