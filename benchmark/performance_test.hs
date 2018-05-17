{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import AnimalClub.Genetics
import System.Random

import Criterion.Main

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
