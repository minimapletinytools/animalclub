import           Relude

import           AnimalClub.Genetics

import           Control.DeepSeq
import           Control.Monad          as Seq
import           Control.Monad.Writer
import qualified Data.Text              as T
import qualified Data.Vector.Storable   as V
import           Data.Word
import           System.Random

import           Formatting
import           Formatting.Clock
import           System.Clock

import qualified Control.Monad.Parallel as Par

-- helpers for examples
-- | Writer monoid for either value or names
type NamedFloats = [(T.Text, [Float])]

-- | Write a single gene values in the builder
tellGene :: (Monad m) => T.Text -> Float -> GenotypeT g NamedFloats m ()
tellGene s v = tellGenes s [v]

-- | Write several gene values in the builder
tellGenes :: (Monad m) => T.Text -> [Float] -> GenotypeT g NamedFloats m ()
tellGenes s v = tell [(s, v)]

-- | example genotype
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

-- basic example of using AnimalClub.Genetics
example1 :: IO ()
example1 = do
  gen <- getStdGen
    --gen = mkStdGen 0
  let
    firstDNA = V.replicate 3 (0x00 :: Word8) --all recessive
    secondDNA = V.replicate 20 (0xFF :: Word8) --all dominant
    thirdDNA = V.replicate 20 (0xAA :: Word8) --all mixed
    bread33 = breed gen thirdDNA thirdDNA
  putStrLn "test DNA"
  forM_
    (zipWith (curry show) [(0 :: Int) ..] [firstDNA, secondDNA, thirdDNA])
    putStrLn
  putStrLn "breeding 2 and 3"
  print bread33
  putStrLn "gene builder test"
  --putStrLn $ show $ evalGeneBuilder (gbNormalizedSum >>= tellGene) (bread33, []) gen
  print $ evalGeneBuilder gbExample bread33 gen




------------------
-- some potato code for testing parallel genotype eval
-- shows that it's not working
-- this really doesn't belong here, TODO move it to its own example file
------------------
splitCount :: Int
splitCount = 100

gbComplicated :: Genotype StdGen [Int] Int
gbComplicated = do
  x <- gbSumRange (0, 99)
  y <- gbTypical (0, 99)
  zs <- forM [(1::Int)..10000] $ const gbNormalizedSum
  return $ round $ x + y + sum zs

gbParExample :: Genotype StdGen [Int] Int
gbParExample = do
  dnal <- gbDNALength
  let
    ml = dnal `quot` splitCount
  vs <- Par.forM [i*ml | i <- [0..(splitCount-1)]] (\x -> usingGene (Gene x ml) gbComplicated)
  return $ sum vs

gbSeqExample :: Genotype StdGen [Int] Int
gbSeqExample = do
  dnal <- gbDNALength
  let
    ml = dnal `quot` splitCount
  vs <- Seq.forM [i*ml | i <- [0..(splitCount-1)]] (\x -> usingGene (Gene x ml) gbComplicated)
  return $ sum vs


forceEvaluateN :: Int -> Genotype StdGen [Int] Int -> DNA -> StdGen -> IO Int
forceEvaluateN c gt dna g = do
  rslt <- Seq.forM [0..c] $ \_ -> return $ force . (\(x,_,_) -> x) $ unGenotype gt dna g
  return (length rslt)


example2 :: IO ()
example2 = do
  g <- getStdGen
  let
    dnal = 1000000
    dna = makeRandDNA g dnal
  s0 <- getTime Monotonic
  dna' <- return (force dna)
  s1 <- getTime Monotonic
  _ <- forceEvaluateN 10000 (gbParExample) dna' g
  s2 <- getTime Monotonic
  _ <- forceEvaluateN 10000 (gbSeqExample) dna' g
  s3 <- getTime Monotonic
  -- these numbers seem to basically be random D:
  fprint (timeSpecs % "\n") s0 s1
  fprint (timeSpecs % "\n") s1 s2
  fprint (timeSpecs % "\n") s2 s3







main :: IO ()
main = do
  example1
  example2
