{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import AnimalClub.Genetics
import qualified AnimalClub.Genetics.Internal.Unused.Genotype as Old
import System.Random

import qualified Control.Monad.Parallel as Par
import qualified Control.Monad as Seq
import Control.Monad.Writer (tell)

import Criterion.Main

splitCount :: Int
splitCount = 40



gbComplicated :: Genotype StdGen [Int] Int
gbComplicated = do
    x <- gbSumRange (0, 99)
    y <- gbTypical (0, 99)
    z <- gbNormalizedSum
    return $ round $ x + y + z


benchgtold :: Int -> Old.Genotype StdGen [Int] [Int]
benchgtold dnal = do
    let
        ml = dnal `quot` splitCount
    Seq.forM [i*ml | i <- [0..(splitCount-1)]] $ \x -> do
        Old.gbPush (Gene x ml)
        r <- gbComplicated
        Old.gbPop
        return r

benchgtseq :: Int -> Genotype StdGen [Int] [Int]
benchgtseq = do
    dnal <- gbDNALength
    let
        ml = dnal `quot` splitCount
    Seq.forM [i*ml | i <- [0..(splitCount-1)]] (\x -> usingGene (Gene x ml) gbComplicated)


benchgtpar :: Int -> Genotype StdGen [Int] [Int]
benchgtpar = do
    dnal <- gbDNALength
    let
        ml = dnal `quot` splitCount
    Par.forM [i*ml | i <- [0..(splitCount-1)]] (\x -> usingGene (Gene x ml) gbComplicated)


main :: IO ()
main = do
    g <- getStdGen
    let
        dnal = 1000000
        dna = makeRandDNA g dnal
    defaultMain [
        bgroup "genotype" [
            bench "serial" $ nf (evalGeneBuilder (benchgtseq >>= tell) dna) g
            ,bench "parallel" $ nf (evalGeneBuilder (benchgtpar >>= tell) dna) g
            ,bench "old" $ nf (Old.evalGeneBuilder (benchgtold dnal >>= tell) (dna, [])) g
            ]
        ]
