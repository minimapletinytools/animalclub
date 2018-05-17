import AnimalClub.Genetics

import Control.Monad as Seq
import Control.Monad.Writer
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import Data.Word
import System.Random
import Control.DeepSeq

import Formatting
import Formatting.Clock
import System.Clock

import qualified Control.Monad.Parallel as Par

-- | Writer monoid for either value or names
type NamedFloats = [(T.Text, [Float])]

-- | Write a single gene values in the builder
tellGene :: (Monad m) => T.Text -> Float -> GenotypeT g NamedFloats m ()
tellGene s v = tellGenes s [v]

-- | Write several gene values in the builder
tellGenes :: (Monad m) => T.Text -> [Float] -> GenotypeT g NamedFloats m ()
tellGenes s v = tell [(s, v)]

gbExample :: (RandomGen g, Monad m) => GenotypeT g NamedFloats m ()
gbExample = do
    gbNormalizedSum >>= tellGene "first"
    gbNormalizedThresh 0.5 >>=
        tellGene "second" .
        (\x ->
             if x
                 then 1
                 else 0)
    a1 <- gbTypical (0, 100)
    a2 <- gbTypical (0, 100)
    tellGenes "third" [a1, a2]
    gbRandomRanges [(0, 1) | _ <- [(0 :: Int) .. 9]] >>= tellGenes "ranges"

splitCount :: Int
splitCount = 100

gbComplicated :: Genotype StdGen [Int] Int
gbComplicated = do
    x <- gbSumRange (0, 99)
    y <- gbTypical (0, 99)
    zs <- forM [1..10000] $ \x -> do gbNormalizedSum 
    return $ round $ x + y + (foldl (+) 0 zs)



gbParExample :: Int -> Genotype StdGen [Int] [Int]
gbParExample dnal = do
    let
        ml = dnal `quot` splitCount
    Par.forM [i*ml | i <- [0..(splitCount-1)]] (\x -> usingGene (Gene x ml) gbComplicated)

gbSeqExample :: Int -> Genotype StdGen [Int] [Int]
gbSeqExample dnal = do
    let
        ml = dnal `quot` splitCount
    Seq.forM [i*ml | i <- [0..(splitCount-1)]] (\x -> usingGene (Gene x ml) gbComplicated)


example1 :: IO ()
example1 = do
    gen <- getStdGen
        --gen = mkStdGen 0
    let firstDNA = V.replicate 3 (0x00 :: Word8) --all recessive
        secondDNA = V.replicate 20 (0xFF :: Word8) --all dominant
        thirdDNA = V.replicate 20 (0xAA :: Word8) --all mixed
        bread33 = breed gen thirdDNA thirdDNA
    putStrLn "test DNA"
    forM_
        (map show (zip [(0 :: Int) ..] [firstDNA, secondDNA, thirdDNA]))
        putStrLn
    putStrLn "breeding 2 and 3"
    print bread33
    putStrLn "gene builder test"
    --putStrLn $ show $ evalGeneBuilder (gbNormalizedSum >>= tellGene) (bread33, []) gen
    print $ evalGeneBuilder gbExample bread33 gen


example2 :: IO ()
example2 = do
    g <- getStdGen
    let
        dnal = 1000000
        dna = makeRandDNA g dnal
    s0 <- getTime Monotonic
    dna' <- return (force dna)
    s1 <- getTime Monotonic
    _ <- return $ evalGeneBuilder (gbParExample dnal >>= tell) dna' g
    s2 <- getTime Monotonic
    _ <- return $ evalGeneBuilder (gbSeqExample dnal >>= tell) dna' g
    s3 <- getTime Monotonic
    -- these numbers seem to basically be random D:
    fprint (timeSpecs % "\n") s0 s1
    fprint (timeSpecs % "\n") s1 s2
    fprint (timeSpecs % "\n") s2 s3


main :: IO ()
main = example2
