{-|
Module      : Gene
Description : Functions and types pertaining to DNA and Genes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

data types pertaining to DNA and Genes.
-}

module AnimalClub.Genetics.DNA (
    -- * Types
    DNA,
    -- * Functions
    makeRandDNA,
    dnaLength,
    breed,
    mutate,
    breedAndMutate
) where

import           Data.Bits
import qualified Data.Vector.Unboxed         as V
import           Data.Word
import           System.Random

import qualified Data.Array.Unboxed           as A

import Control.Exception.Base (assert)


-- we don't use repa because repa uses Vector.Unboxed as its underlying type which is not tightly packed

-- TODO replace with UArray Word32 Bool D: it's tightly packed like you want... fml
--type DNA = A.UArray Bool

-- | Allele4 is 4 single gene pairs (allele represented as a Word8
-- not really necessary type synonym tbh
type Allele4 = Word8

-- | DNA host genetic information for all gene transformations in this module
-- DNA is an array of 'Allele4'
-- TODO switch to Repa if you really want parallelizable tightly packed bit arrays
-- TODO consider newtyping this
type DNA = V.Vector Allele4

-- | number of single gene pairs in DNA
-- TODO rename to geneCount
dnaLength :: DNA -> Int
dnaLength dna = 4 * V.length dna

-- | create DNA of all 0s with given dnaLength
-- length must be multiple of 4
makeZeroDNA :: Int -> DNA
makeZeroDNA c = assert (c `mod` 4 == 0) $ V.generate c (const 0)

-- | create random DNA with given dnaLength
-- length must be multiple of 4
makeRandDNA :: (RandomGen g) => g -> Int -> DNA
makeRandDNA g c = assert (c `mod` 4 == 0) $ V.unfoldrN (c `div` 4) (Just . random) g

breed :: (RandomGen g) => g -> DNA -> DNA -> DNA
breed g a b = V.map choose (V.zip3 a b rands) where
    rands = V.fromList . take (V.length a) . randoms $ g
    choose :: (Allele4, Allele4, Word8) -> Allele4
    choose (geneA, geneB, r) = mated where
        ar = r .&. 0xAA
        car = complement r .&. 0xAA
        br = r .&. 0x55
        cbr = complement r .&. 0x55
        ac = (ar .&. geneA) .|. (car .&. unsafeShiftL geneA 1)
        bc = (br .&. geneB) .|. (cbr .&. unsafeShiftR geneB 1)
        mated = ac .|. bc

mutateBit :: (RandomGen g) => g -> Word8 -> Word8
mutateBit g x = unsafeShiftL 0x01 (fst $ randomR (0,7) g) `xor` x

-- |
-- chance is chance of one bit mutating per byte
mutate :: (RandomGen g) => Float -> g -> DNA -> DNA
mutate chance g dna = V.zipWith zipFunc rands dna where
    -- TODO running random twice here is very inefficient
    -- you really want to mapAccumL a single generator over the dna
    -- and only evaluate a second time when there is a mutation
    rands = V.fromList . take (V.length dna) . randoms $ g
    zipFunc gx x = r where
        (c, gx') = randomR (0,1.0) (mkStdGen gx)
        r = if c < chance then mutateBit gx' x else x

breedAndMutate :: (RandomGen g) => Float -> g -> DNA -> DNA -> DNA
breedAndMutate chance g a b = dna where
    (g',g'') = split g
    dna' = breed g' a b
    dna = mutate chance g'' dna'

{-
mutateBit :: (RandomGen g) => g -> Word8 -> Word8
mutateBit g x = unsafeShiftL 0x01 (randomR (0,7) g) `xor` x

-- doesn't work because vector not traversable
-- |
-- chance is chance of one bit mutating per byte
mutate :: (RandomGen g) => Float -> g -> DNA -> DNA
mutate chance g dna = final where
    mutateFunc acc x = (acc', r) where
        (c, acc') = randomR (0,1.0) acc
        r = if c < chance then mutateBit acc' x else x
    (_, final) = mapAccumL mutateFunc g dna
-}

-- | breed 2 DNA together, randomly taking 1 allele from each parent for each gene
-- single genes come in bit pairs. Breeding a single gene randomly takes one bit from each parent gene
{-breed :: (RandomGen gen) => gen -> DNA -> DNA -> DNA
breed g a b = V.map choose (V.zip3 a b (V.generate (V.length a) id)) where
    rands = randoms g
    choose (geneA, geneB, n) = mated where
        choiceL :: Word8
        choiceL = foldl (\acc x -> shiftL acc 2 .|. if x then 0x01 else 0x02) 0x00 $ take 4 . drop (n*8) $ rands
        choiceR :: Word8
        choiceR = foldl (\acc x -> shiftL acc 2 .|. if x then 0x01 else 0x02) 0x00 $ take 4 . drop (n*8+4) $ rands
        --shiftBitAtIndex: left or right, word to shift, index to shift
        --e.g. shiftBitAtIndex True (01010101) 2 = (00001000)
        shiftBitAtIndex::Bool -> Word8 -> Int -> Word8
        shiftBitAtIndex l x i = if doShift then shifted else bits where
            shiftDir = if l then shiftL else shiftR
            doShift = (shiftL (if l then 0x02 else 0x01) i) .&. x == (0x00::Word8) --shift if we do not find a 1 in left position
            bits::Word8
            bits = x .&. (shiftL 0x03 i) --grab the desired bits in the desired spot
            shifted = shiftDir bits 1 --shift the bytes
        chosenL = foldl (\acc x -> acc .|. shiftBitAtIndex True (geneA .&. choiceL) x) 0x00 [2*x | x <-[0..3]]
        chosenR = foldl (\acc x -> acc .|. shiftBitAtIndex False (geneB .&. choiceR) x) 0x00 [2*x | x <-[0..3]]
        mated::Word8
        --mated = trace (show chosenR ++ " " ++ show chosenL) $ chosenL .|. chosenR
        mated = chosenL .|. chosenR-}