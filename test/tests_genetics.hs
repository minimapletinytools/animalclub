{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

import AnimalClub.Genetics

import Control.Monad.Writer (tell)
import Data.List
import             Data.Semigroup (Semigroup, (<>))
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import Data.Word
import System.Random
import Test.QuickCheck

import Debug.Trace

-- |
instance Arbitrary (DNA) where
    arbitrary =
        sized
            (\n -> V.generateM n (\_ -> arbitraryBoundedIntegral :: Gen Word8))

-- | Writer monoid for either value or names
type NamedFloats = [(T.Text, [Float])]

-- | Write a single gene values in the builder
tellGene :: (Monad m) => T.Text -> Float -> GenotypeT g NamedFloats m ()
tellGene s v = tellGenes s [v]

-- | Write several gene values in the builder
tellGenes :: (Monad m) => T.Text -> [Float] -> GenotypeT g NamedFloats m ()
tellGenes s v = tell $ [(s, v)]

dummyGen = mkStdGen 0

extractFirstValue = head . snd . head

{- Breeding tests -}
prop_basicbreedingtest :: Bool
prop_basicbreedingtest =
    let n = 10
        firstDNA = V.replicate n (0x00 :: Word8) --all recessive
        secondDNA = V.replicate n (0xFF :: Word8) --all dominant
        gen = mkStdGen 0
        bread = breed gen firstDNA secondDNA
        sm =
            extractFirstValue $
            evalGeneBuilder
                (gbSum >>= tellGene "" . fromIntegral)
                (bread, [])
                dummyGen
    in sm == fromIntegral n * 4

{- GeneBuilder tests -}
prop_gbNormalizedSum_test :: DNA -> Bool
prop_gbNormalizedSum_test dna = (o >= 0.0) && (o <= 1.0)
  where
    o =
        extractFirstValue $
        evalGeneBuilder (gbNormalizedSum >>= tellGene "") (dna, []) dummyGen

prop_gbSum_test :: DNA -> Bool
prop_gbSum_test dna = (o >= 0.0) && (o <= l)
  where
    l = fromIntegral $ 8 * V.length dna
    o =
        extractFirstValue $
        evalGeneBuilder
            (gbSum >>= tellGene "" . fromIntegral)
            (dna, [])
            dummyGen

prop_gbTypical :: DNA -> (Float, Float) -> Bool
prop_gbTypical dna (a', b') = (o >= a) && (o <= b)
  where
    a = min a' b'
    b = max a' b'
    o =
        extractFirstValue $
        evalGeneBuilder (gbTypical (a, b) >>= tellGene "") (dna, []) dummyGen

prop_gbRandomRanges :: DNA -> [(Float, Float)] -> Bool
prop_gbRandomRanges dna' ranges' = pass
  where
    ranges = map (\(a, b) -> (min a b, max a b)) ranges'
    rl = length ranges
    dna'' = V.cons 0x00 dna'
    --replicate our definitely non-empty dna a bunch of times until it's long enough
    makedna x =
        if 4 * V.length x < 8 * rl
            then makedna (x V.++ dna'')
            else x
    dna = makedna dna''
    o =
        mconcat . map snd $
        evalGeneBuilder
            (gbRandomRanges ranges >>= mapM (tellGene ""))
            (dna, [])
            dummyGen
    pass = all (\((mn, mx), x) -> (x >= mn) && (x <= mx)) $ zip ranges o

instance Semigroup Float where
    (<>) = (+)

instance Monoid Float where
    mempty = 0
    mappend = (<>)

prop_convergence :: Int -> Bool
prop_convergence seed = pass
  where
    g1 = mkStdGen seed
    (target :: Float, g2) = randomR (0, 100) g1
    maxGenerations = 200
    dnaLength_ = 25
    thresh = 0.25
    original = makeRandDNA g1 dnaLength_
    testgene :: Genotype StdGen Float ()
    testgene = gbTypical (-20, 120) >>= tell
    test dna = evalGeneBuilder testgene (dna, []) (mkStdGen 0)
    unfoldWormF (dnas, g) =
        if testResult < thresh
            then Nothing
            else Just $ (next_dnas, acc)
      where
        acc@(next_dnas, _) = breedAndSelectPool (test) 0.003 g (15, 2) dnas
        testResult = test $ head next_dnas
    generations =
        length $ take maxGenerations $ unfoldr unfoldWormF ([original], g2)
    --pass = trace ("took " ++ show generations ++ " to pass " ++ (show seed) ++ "\n") $ generations < maxGenerations
    pass = generations < maxGenerations

--Template haskell nonsense to run all properties prefixed with "prop_" in this file
return []

main = $quickCheckAll
--main = $verboseCheckAll
