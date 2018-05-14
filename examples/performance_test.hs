-- TODO you should probably just delete this file..

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Worm

import AnimalClub.Animals
import AnimalClub.Genetics
import AnimalClub.Skellygen
import AnimalClub.Skellygen.Math.Mesh

import Text.Printf (printf)
import           Data.List                       (unfoldr)
import System.Random

--import System.Clock
--import qualified Debug.Trace as Debug


{-clockSomething :: a -> IO ()
clockSomething something = do
    start <- getTime Monotonic
    void (evaluate something)
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end-}

main :: IO ()
main = do
    segs <- return 20
    dnaPerSeg <- return 100
    printf "Running tests. segs: %i, dna per seg: %i " segs dnaPerSeg :: IO ()
    gen1 <- getStdGen
    let
        (_, gen2) = next gen1
        genome = (wormGenome segs dnaPerSeg)
        original = makeRandDNA gen1 (segs * dnaPerSeg)
        unfoldWormF (dnas, g) = Just $ (next_dnas, acc) where
            acc@(next_dnas, _) = breedAndSelectWormPool (testWorm segs) genome 0.003 g (10,3) dnas
        bestWorms = last $ take 300 $ unfoldr unfoldWormF ([original], gen2)
        bestWorm = last bestWorms
        bestWormProps = generateAnimalProperties $ evalGenome genome bestWorm
        skelly = animalNodeToSkellyNodeWithProps bestWormProps (worm segs)
    writeFile "wigglyworm.obj" . meshToObj . generateMesh $ skelly
