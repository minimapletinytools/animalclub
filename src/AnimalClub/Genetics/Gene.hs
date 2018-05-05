{-|
Module      : Gene
Description : Functions and types pertaining to Genes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

Functions and types pertaining to Genes
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
--{-# LANGUAGE FunctionalDependencies                 #-} -- needed so we can do (KnownNat n) => ... in class Gene
--{-# LANGUAGE ScopedTypeVariables                 #-} -- needed to access n of type sig in geneLength

module AnimalClub.Genetics.Gene (
    Gene(..),
    combineGene,
    tryGeneSum,
    geneLength,
    geneSum
) where

import           AnimalClub.Genetics.DNA

import qualified Data.Vector.Unboxed as V
import           Data.Bits
import Data.List (foldl')

-- | sum all bits of the genotype given its host DNA
-- throws an error if genotype is not a valid index subset of the dna
tryGeneSum ::  DNA -> Gene -> Int
tryGeneSum dna gt =
    if geneLength gt > dnaLength dna
        then error $ "genotype longer than dna " ++ (show $ geneLength gt) ++ " " ++ (show $ 4 * V.length dna)
        else geneSum dna gt

-- | represents a genotype as a start index and length
data Gene = Gene {
    startGene :: Int,
    geneCount :: Int
} deriving (Show)

-- | combine 2 Genes
-- where gt2 is sub index set of gt1
combineGene :: Gene -- ^ child
    -> Gene -- ^ parent
    -> Gene -- ^ combined
combineGene gt2 gt1 =
    if startGene gt2 + geneCount gt2 > geneCount gt1
        then error $ "inconsistency " ++ (show $ gt1) ++ " " ++ (show $ gt2)
        else Gene (startGene gt1 + startGene gt2) (geneCount gt2)

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

-- | length of genotype
geneLength :: Gene -> Int
geneLength = geneCount

-- | sum of all bit pairs in this genotype
geneSum :: DNA -> Gene -> Int
geneSum dna gt = fastGeneSumInternal dna (startGene gt) (geneCount gt)
