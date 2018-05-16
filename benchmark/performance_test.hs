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

import Criterion.Main

--import qualified Debug.Trace as Debug


mutatevsmutateold :: IO ()
mutatevsmutateold = do
    let
        lns = [100, 1000, 10000]
    stdg <- getStdGen
    let
        dnas = map (makeRandDNA stdg) lns
    defaultMain [
        bgroup "StdGen mutate 0.1" $ map (\(l, dna) -> bench (show l) $ nf (mutate 0.1 stdg) dna) (zip lns dnas)
        ,bgroup "StdGen mutateOld 0.1" $ map (\(l, dna) -> bench (show l) $ nf (mutateOld 0.1 stdg) dna) (zip lns dnas)
        ]

main :: IO ()
main = mutatevsmutateold
