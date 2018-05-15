-- TODO you should probably just delete this file..

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Worm

import AnimalClub.Animals
import AnimalClub.Genetics
import AnimalClub.Skellygen
import AnimalClub.Skellygen.Math.Mesh
import qualified AnimalClub.Genetics.DNAMWC as MWC

import Text.Printf (printf)
import           Data.List                       (unfoldr)
import System.Random

import Criterion.Main
import qualified System.Random.MWC as MWC

--import System.Clock
--import qualified Debug.Trace as Debug


{-clockSomething :: a -> IO ()
clockSomething something = do
    start <- getTime Monotonic
    void (evaluate something)
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end-}

mwcvsstd :: IO ()
mwcvsstd = do
    let
        lns = [100, 1000, 10000]
    g <- MWC.create
    stdg <- getStdGen
    dnas <- mapM (MWC.makeRandDNA g) lns
    defaultMain [
        bgroup "MWC create" $ map (\l -> bench (show l) $ nfIO (MWC.makeRandDNA g l)) lns
        ,bgroup "StdGen create" $ map (\l -> bench (show l) $ nf (makeRandDNA stdg) l) lns
        ,bgroup "MWC breed" $ map (\(l, dna) -> bench (show l) $ nfIO (MWC.breed g dna dna)) (zip lns dnas)
        ,bgroup "StdGen breed" $ map (\(l, dna) -> bench (show l) $ nf (breed stdg dna) dna) (zip lns dnas)
        ,bgroup "MWC mutate 0.1" $ map (\(l, dna) -> bench (show l) $ nfIO (MWC.mutate 0.1 g dna)) (zip lns dnas)
        ,bgroup "StdGen mutate 0.1" $ map (\(l, dna) -> bench (show l) $ nf (mutate 0.1 stdg) dna) (zip lns dnas)
        ]

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
