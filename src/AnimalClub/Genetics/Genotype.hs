{-|
Module      : Genotype
Description : Functions and types pertaining to Genotypes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

Functions and types pertaining to Genotypes
-}

--{-# LANGUAGE AllowAmbiguousTypes       #-} -- defers type check until function is actually used, I don't really understand this
--{-# LANGUAGE DataKinds                 #-} -- so I can do data ___ (n::Nat)
--{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE ExplicitNamespaces        #-}
--{-# LANGUAGE FlexibleContexts          #-}
--{-# LANGUAGE FlexibleInstances         #-} -- needed for instance declaration, not sure why I need this
--{-# LANGUAGE GADTs                     #-} -- so I can do fancy type constraints?
--{-# LANGUAGE KindSignatures            #-} -- so i can do (n::Nat)
--{-# LANGUAGE MultiParamTypeClasses     #-} -- so I can do a multiparameter type class
--{-# LANGUAGE TypeOperators             #-}
--{-# LANGUAGE RankNTypes                 #-}
--{-# LANGUAGE InstanceSigs                 #-} -- idk
--{-# LANGUAGE FunctionalDependencies                 #-} -- needed so we can do (KnownNat n) => ... in class Genotype
--{-# LANGUAGE ScopedTypeVariables                 #-} -- needed to access n of type sig in geneLength

module AnimalClub.Genetics.Genotype (
    Genotype(..),
    FastGenotype(..),
    combineFastGenotype,
    tryGeneSum
) where

import           AnimalClub.Genetics.Gene

import qualified Data.Vector.Unboxed as V
import           Data.Bits
import Data.List (foldl')

-- | interface for Genotype, a hierarchical index sub sets of DNA
-- currently this class only has one instance.
-- in principle, all genotype operations could be implementation independent
class Genotype gt where
    -- | length of genotype
    geneLength :: gt -> Int
    -- | sum of all bit pairs in this genotype
    geneSum :: DNA -> gt -> Int
    --combine :: (Genotype m gt2, m <= n) => gt -> gt2 -> gt2

-- | sum all bits of the genotype given its host DNA
-- throws an error if genotype is not a valid index subset of the dna
tryGeneSum :: (Genotype g) => DNA -> g -> Int
tryGeneSum dna gt =
    if geneLength gt > dnaLength dna
        then error $ "genotype longer than dna " ++ (show $ geneLength gt) ++ " " ++ (show $ 4 * V.length dna)
        else geneSum dna gt

-- | represents a genotype as a start index and length
data FastGenotype = FastGenotype {
    startGene :: Int,
    geneCount :: Int
} deriving (Show)

-- | combine 2 FastGenotypes
-- where gt2 is sub index set of gt1
combineFastGenotype :: FastGenotype -- ^ child
    -> FastGenotype -- ^ parent
    -> FastGenotype -- ^ combined
combineFastGenotype gt2 gt1 =
    if startGene gt2 + geneCount gt2 > geneCount gt1
        then error $ "inconsistency " ++ (show $ gt1) ++ " " ++ (show $ gt2)
        else FastGenotype (startGene gt1 + startGene gt2) (geneCount gt2)

-- dna start count -> sum
fastGeneSumInternal :: DNA -> Int -> Int -> Int
fastGeneSumInternal dna start cnt = snd $ fastGeneSumInternal' dna start (cnt, 0)

-- dna start (count, accum) -> (remaining count, sub sum)
fastGeneSumInternal' :: DNA -> Int -> (Int, Int) -> (Int, Int)
fastGeneSumInternal' _ _ (0, a) = (0, a)
fastGeneSumInternal' dna i (cnt, a) =
    let
        startIndex = i `quot` 4
        startBitDiv2 = (i `mod` 4)
        totalBits = min cnt (4 - startBitDiv2)
        word = shiftR (dna V.! startIndex) (2 * startBitDiv2)
        subSum = fromInteger . toInteger $ foldl' (\acc x -> acc + 0x01 .&. shiftR word x) 0x00 [0..(totalBits*2-1)]
    in
        fastGeneSumInternal' dna (i + totalBits) (cnt - totalBits, a + subSum)

instance Genotype FastGenotype where
    geneSum dna gt = fastGeneSumInternal dna (startGene gt) (geneCount gt)
    geneLength = geneCount


-- | NOT SUPPORTED YET AND PROBABLY NEVER WILL BE
-- it's nice in principle but the performance is terrible
-- and I doubt it adds any unique expressions
-- we could do
-- combine :: (Genotype g) => g -> g -> GeneralGenotype
-- but this would incur a performance hit in the case of
-- FastGenotype -> FastGenotype -> GeneralGenotype
data GeneralGenotype = GeneralGenotype {
    indices :: V.Vector Int
}

instance Genotype GeneralGenotype where
    geneSum dna = fromInteger . toInteger . V.sum . V.map (\i -> dna V.! i) . indices
    geneLength = V.length . indices
