{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import           Relude

import           AnimalClub.Genetics

import           System.Random

import           Criterion.Main



main :: IO ()
main = do
  let
    lns = [100, 1000, 10000]
  stdg <- getStdGen
  let
    (g1,g2) = split stdg
    dnas = map (makeRandDNA g1) lns
    dnas2 = map (makeRandDNA g2) lns
  defaultMain [
    bgroup "StdGen create" $ map (\l -> bench (show l) $ nf (makeRandDNA stdg) l) lns
    ,bgroup "StdGen breed" $ map (\(l, dna1, dna2) -> bench (show l) $ nf (breed stdg dna1) dna2) (zip3 lns dnas dnas2)
    ,bgroup "StdGen mutate 0.1" $ map (\(l, dna) -> bench (show l) $ nf (mutate 0.1 stdg) dna) (zip lns dnas)
    --,bgroup "StdGen mutateOld 0.1" $ map (\(l, dna) -> bench (show l) $ nf (mutateOld 0.1 stdg) dna) (zip lns dnas)
    ]
