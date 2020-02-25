{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import           AnimalClub.Genetics
import qualified AnimalClub.Genetics.Internal.Unused.Genotype as Old
import           System.Random

import qualified Control.Monad                                as Seq
import qualified Control.Monad.Parallel                       as Par
import           Control.Monad.State                          (get)
import           Control.Monad.Writer                         (tell)

import           Criterion.Main

import           Control.DeepSeq
import           Data.Time.Clock

splitCount :: Int
splitCount = 40



gbComplicated :: Genotype StdGen [Int] Int
gbComplicated = do
    x <- gbSumRange (0, 99)
    y <- gbTypical (0, 99)
    z <- gbNormalizedSum
    return $ round $ x + y + z

benchgtseq :: Genotype StdGen [Int] [Int]
benchgtseq = do
    dnal <- gbDNALength
    let
        ml = dnal `quot` splitCount
    Seq.forM [i*ml | i <- [0..(splitCount-1)]] (\x -> usingGene (Gene x ml) gbComplicated)

benchgtpar :: Genotype StdGen [Int] [Int]
benchgtpar = do
    dnal <- gbDNALength
    let
        ml = dnal `quot` splitCount
    Par.forM [i*ml | i <- [0..(splitCount-1)]] (\x -> usingGene (Gene x ml) gbComplicated)

markTime :: String -> UTCTime -> IO UTCTime
markTime s t = do
    t' <- getCurrentTime
    putStrLn $ s ++ ": " ++ show (diffUTCTime t' t)
    return t'

main :: IO ()
main = do
    g <- getStdGen
    let
        dnal = 10000000
        dna = makeRandDNA g dnal
    dna `deepseq` return ()


    -- simplified benchmarks

    -- making correct # of sparks as expected but sparks almost all get GC or fizzled
    t1 <- getCurrentTime
    r1 <- return $ evalGeneBuilder (benchgtpar >>= tell) dna g
    r1 `deepseq` return ()
    t2 <- markTime "par" t1

    r2 <- return $ evalGeneBuilder (benchgtseq >>= tell) dna g
    r2 `deepseq` return ()
    t3 <- markTime "seq" t2

    return ()


    -- criterion benchmarks, disabled for now until I get the above to work properly
    {-
    defaultMain [
        bgroup "genotype" [
            bench "serial" $ nf (evalGeneBuilder (benchgtseq >>= tell) dna) g
            ,bench "parallel" $ nf (evalGeneBuilder (benchgtpar >>= tell) dna) g
            ,bench "old" $ nf (Old.evalGeneBuilder (benchgtold >>= tell) (dna, [])) g
            ]
        ]-}
