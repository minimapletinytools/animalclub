--{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Worm

import AnimalClub.Animal.Animal
import AnimalClub.Genetics
import AnimalClub.Skellygen
import AnimalClub.Skellygen.Math.Mesh

import Text.Printf (printf)
import           Data.List                       (unfoldr)
import System.Random


import qualified Debug.Trace as Debug


main :: IO ()
main = do
    segs <- return 20
    dnaPerSeg <- return 120
    mutationChance <- return 0.003
    litterSize <- return 30
    selectionSize <- return 3
    generations <- return 300
    printf "Breeding worms. segs: %i, dna per seg: %i, mutation: %f \n" segs dnaPerSeg mutationChance :: IO ()
    printf "litter: %i, selection: %i, generations: %i \n" litterSize selectionSize generations :: IO ()
    gen1 <- getStdGen
    let
        genome = (wormGenome segs dnaPerSeg)
        (_, gen2) = next gen1
        original = makeRandDNA gen1 (segs * dnaPerSeg)
        unfoldWormF (dnas, g) = Just $ (next_dnas, acc) where
            acc@(next_dnas, _) = breedAndSelectWormPool (testWorm segs) genome mutationChance g (litterSize,selectionSize) dnas
        bestWorms = last $ take generations $ unfoldr unfoldWormF ([original], gen2)
        bestWorm = last bestWorms
        bestWormProps = generateAnimalProperties $ evalGenome genome bestWorm
        skelly = animalNodeToSkellyNodeWithProps bestWormProps (worm segs)
    writeFile "wigglyworm.obj" . meshToObj . generateMesh $ skelly
