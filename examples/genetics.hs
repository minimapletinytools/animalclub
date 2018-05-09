import AnimalClub.Genetics

import Control.Monad
import Control.Monad.Writer
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import Data.Word
import System.Random

-- | Writer monoid for either value or names
type NamedFloats = [(T.Text, [Float])]

-- | Write a single gene values in the builder
tellGene :: (Monad m) => T.Text -> Float -> GenotypeT g NamedFloats m ()
tellGene s v = tellGenes s [v]

-- | Write several gene values in the builder
tellGenes :: (Monad m) => T.Text -> [Float] -> GenotypeT g NamedFloats m ()
tellGenes s v = tell $ [(s, v)]

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

main :: IO ()
main = do
    gen <- getStdGen
        --gen = mkStdGen 0
    let firstDNA = V.replicate 3 (0x00 :: Word8) --all recessive
        secondDNA = V.replicate 20 (0xFF :: Word8) --all dominant
        thirdDNA = V.replicate 20 (0xAA :: Word8) --all mixed
        bread33 = breed gen thirdDNA thirdDNA
    putStrLn $ "test DNA"
    forM_
        (map show (zip [(0 :: Int) ..] [firstDNA, secondDNA, thirdDNA]))
        putStrLn
    putStrLn $ "breeding 2 and 3"
    putStrLn $ show $ bread33
    putStrLn $ "gene builder test"
    --putStrLn $ show $ evalGeneBuilder (gbNormalizedSum >>= tellGene) (bread33, []) gen
    putStrLn $ show $ evalGeneBuilder gbExample (bread33, []) gen
{- -}
--gbExample = do
    --make a genotype based on a percentage of current
    --makeGene 0.25 >>= gbPush
