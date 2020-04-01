{-|
Module      : Gene
Description : Functions and types pertaining to Genes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

Functions and types pertaining to Genes
-}

module AnimalClub.Genetics.Gene (
  Gene(..),
  combineGene,
  extractDNA,
  geneLength,
  geneSum,
  geneBitCount
) where

import           Relude

import           AnimalClub.Genetics.DNA

import qualified Data.Vector.Generic     as G
import qualified Data.Vector.Storable    as V

-- | represents a genotype as a start index and length
-- indices are on 8-bit (Word8) intervals
data Gene = Gene {
  _start :: Int,
  _count :: Int
} deriving (Show)

-- | combine 2 Genes
-- where gt2 is sub index set of gt1
combineGene :: Gene -- ^ child
  -> Gene -- ^ parent
  -> Gene -- ^ combined
combineGene gt2 gt1 =
  if _start gt2 + _count gt2 > _count gt1
    then error $ "parent child mismatch " <> (show $ gt1) <> " " <> (show $ gt2)
    else Gene (_start gt1 + _start gt2) (_count gt2)

-- | extractDNA extracts a Gene from DNA producing a new DNA that is the subsection of the original DNA as defined by the Gene
-- will throw an error if Gene is out of bounds of DNA being operated on
extractDNA ::
 Gene -- ^ Gene to extract
 -> DNA -- ^ DNA to extract from
 -> DNA
extractDNA (Gene i n) = G.slice i n

{-
-- |
-- this version uses indices on Allele (2 bit) boundaries
fastGeneSumInternalAlleles :: DNA -> Int -> Int -> Int
fastGeneSumInternalAlleles dna start cnt = snd $ fastGeneSumInternal' dna start (cnt, 0)

-- |
-- this version uses indices on Allele (2 bit) boundaries
fastGeneSumInternalAlleles' :: DNA -> Int -> (Int, Int) -> (Int, Int)
fastGeneSumInternalAlleles' _ _ (0, a) = (0, a)
fastGeneSumInternalAlleles' dna i (cnt, a) =
  let
    startIndex = i `quot` 4
    startBitDiv2 = (i `mod` 4)
    totalBits = min cnt (4 - startBitDiv2)
    word = shiftR (dna G.! startIndex) (2 * startBitDiv2)
    subSum = fromInteger . toInteger $ foldl' (\acc x -> acc + 0x01 .&. shiftR word x) 0x00 [0..(totalBits*2-1)]
  in
    fastGeneSumInternalAlleles' dna (i + totalBits) (cnt - totalBits, a + subSum)
-}

-- | length of genotype in bytes
geneLength :: Gene -> Int
geneLength = _count

-- | sum all bits of the genotype given its host DNA
-- throws an error if genotype is not a valid index subset of the dna
{-tryGeneSum ::  DNA -> Gene -> Int
tryGeneSum dna gt =
  if _start gt + geneLength gt > dnaLength dna
    then error $ "genotype longer than dna " ++ (show $ geneLength gt) ++ " " ++ (show $ dnaLength dna)
    else geneSum dna gt-}

-- | sum of all bit pairs in this genotype
geneSum :: DNA -> Gene -> Int
geneSum dna gt = dnaSum $ G.slice (_start gt) (_count gt) dna

-- | returns an 8 length array that counts occurrence of each bit
geneBitCount :: DNA -> Gene -> V.Vector Int
geneBitCount dna gt = dnaBitCount $ G.slice (_start gt) (_count gt) dna
