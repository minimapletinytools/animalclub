{-# OPTIONS_GHC -fno-warn-orphans #-}

module AnimalClub.GeneticsSpec (
  spec
) where

import           Relude               hiding (head)
import           Relude.Unsafe        (head, (!!))

import           AnimalClub.Genetics

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.Writer (tell)
import           Data.Bits
import           Data.Semigroup       (Semigroup, (<>))
import qualified Data.Vector.Storable as V
import           Data.Word
import           System.Random
import qualified Text.Show



-- |
instance Arbitrary (DNA) where
    arbitrary =
        sized
            (\n -> V.generateM n (\_ -> arbitraryBoundedIntegral :: Gen Word8))

-- | Writer monoid for either value or names
type NamedFloats = [(Text, [Float])]

-- | Write a single gene values in the builder
tellGene :: (Monad m) => Text -> Float -> GenotypeT g NamedFloats m ()
tellGene s v = tellGenes s [v]

-- | Write several gene values in the builder
tellGenes :: (Monad m) => Text -> [Float] -> GenotypeT g NamedFloats m ()
tellGenes s v = tell $ [(s, v)]

dummyGen :: StdGen
dummyGen = mkStdGen 0

extractFirstValue :: [(a, [b])] -> b
extractFirstValue = head . snd . head

{- Breeding tests -}
basicbreedingtest :: Expectation
basicbreedingtest =
    let n = 10
        firstDNA = V.replicate n (0x00 :: Word8) --all recessive
        secondDNA = V.replicate n (0xFF :: Word8) --all dominant
        gen = mkStdGen 0
        bread = breed gen firstDNA secondDNA
        sm =
            extractFirstValue $
            evalGeneBuilder
                (gbSum >>= tellGene "" . fromIntegral)
                bread
                dummyGen
    in sm `shouldBe` fromIntegral n * 4

{- GeneBuilder tests -}
prop_gbNormalizedSum_test :: DNA -> Bool
prop_gbNormalizedSum_test dna = (o >= 0.0) && (o <= 1.0)
  where
    o =
        extractFirstValue $
        evalGeneBuilder (gbNormalizedSum >>= tellGene "") dna dummyGen

prop_gbSum_test :: DNA -> Bool
prop_gbSum_test dna = (o >= 0.0) && (o <= l)
  where
    l = fromIntegral $ 8 * V.length dna
    o =
        extractFirstValue $
        evalGeneBuilder
            (gbSum >>= tellGene "" . fromIntegral)
            dna
            dummyGen

prop_gbTypical :: DNA -> (Float, Float) -> Bool
prop_gbTypical dna (a', b') = (o >= a) && (o <= b)
  where
    a = min a' b'
    b = max a' b'
    o =
        extractFirstValue $
        evalGeneBuilder (gbTypical (a, b) >>= tellGene "") dna dummyGen

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
            dna
            dummyGen
    pass = all (\((mn, mx), x) -> (x >= mn) && (x <= mx)) $ zip ranges o

prop_gbByteSample1 :: Word8 -> Bool
prop_gbByteSample1 w = all id $ map (\i -> getBit w i * n == r !! i) [0..7] where
    getBit a i = if testBit a i then 1 else 0
    n = 100
    dna = V.replicate n w
    r = evalGeneBuilder
            (gbByteSample1 >>= tell)
            dna
            dummyGen



{-
-- | returns a 4 length array that counts occurrence of [00,01,10,11]
-- on non-overlapping intervals of 2 bits
-- TODO
prop_gbByteSample2 :: DNA -> Bool
prop_gbByteSample2 dna = True
-}


-- |
{-
prop_gbBytePattern4 :: Word8 -> Bool
prop_gbBytePattern4 w = r == 100 * double where
    double = if 0xF (Data.Bits..&.) w == shiftR w 4 then 2 else 1
    dna = V.replicate 100 w
    r = extractFirstValue $
        evalGeneBuilder
            (gbBytePattern4 w >>= tellGene "" . fromIntegral)
            dna
            dummyGen
-}

prop_gbBytePattern_test1 :: Word8 -> Bool
prop_gbBytePattern_test1 w = r == 100 where
    dna = V.replicate 100 w
    r = extractFirstValue $
        evalGeneBuilder
            (gbBytePattern w >>= tellGene "" . fromIntegral)
            dna
            dummyGen

instance Semigroup Float where
    (<>) = (+)

instance Monoid Float where
    mempty = 0
    mappend = (<>)

prop_convergence :: Int -> Bool
prop_convergence seed = pass
  where
    -- simple gene that we will use to test if breeding repeatedly can let it converge to some value
    testgene :: Genotype StdGen Float ()
    testgene = gbTypical (-20, 120) >>= tell

    -- pick a random targe value to converge to
    g1 = mkStdGen seed
    (target :: Float, g2) = randomR (0, 100) g1

    -- hardcoded parameters
    maxGenerations = 200
    dnaLength_ = 25
    thresh = 0.25

    -- start with a random DNA
    original = makeRandDNA g1 dnaLength_

    -- define our fitness function
    fitness :: FitnessFunc
    fitness dna = evalGeneBuilder testgene dna (mkStdGen 0) - target

    -- breed until our fitness function above meets the threshold
    unfoldWormF (dnas, g) =
        if testResult < thresh
            then Nothing
            else Just (next_dnas, acc)
      where
        acc@(next_dnas, _) = breedAndSelectPool fitness 0.003 g (15, 2) dnas
        testResult = fitness $ head next_dnas

    -- count the number of generations it took to converge
    generations =
        length $ take maxGenerations $ unfoldr unfoldWormF ([original], g2)

    -- pass if we took fewer than maxGenerations to converge
    --pass = trace ("took " ++ show generations ++ " to pass " ++ (show seed) ++ "\n") $ generations < maxGenerations
    pass = generations < maxGenerations


spec :: Spec
spec = do
  describe "Genetics" $ do
    describe "Breeding" $ do
      it "satisfied passes basic breeding test" $
        basicbreedingtest
    describe "GeneBuilder" $ do
      it "gbNormalizedSum" $
        property $ prop_gbNormalizedSum_test
      it "gbSum" $
        property $ prop_gbSum_test
      it "gbTypical" $
        property $ prop_gbTypical
      it "gbRandomRanges" $
        property $ prop_gbRandomRanges
      it "gbByteSample1" $
        property $ prop_gbByteSample1
      it "gbBytePattern" $
        property $ prop_gbBytePattern_test1
    it "satsifies convergence property" $
      property $ prop_convergence
