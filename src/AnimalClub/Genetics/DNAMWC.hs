{-|
Module      : DNAMWC
Description : DNA methods implemented using mwc-random
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

This module has the same functionality as AnimalClub.Genetics.DNA.DNAMWC except implemented using random-mwc
The signatures are adjusted accordingly.
-}

module AnimalClub.Genetics.DNAMWC (

    -- * These have different typse
    makeRandDNA,
    breed,
    mutate,
    breedAndMutate,
    breedAndSelectPool
) where

import qualified AnimalClub.Genetics.DNA as DNA
import           Data.Bits
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import Data.List (sortBy)
import           Data.Ord                        (comparing)
import           Data.Word
import System.Random.MWC

import Control.Monad.Primitive
import Control.Exception.Base (assert)

-- | create random DNA with given dnaLength
-- length must be multiple of 4
makeRandDNA :: (PrimMonad m) => Gen (PrimState m) -> Int -> m DNA.DNA
makeRandDNA g c = assert (c `mod` 4 == 0) $ do
    uniformVector g (c `div` 4)

-- | breed 2 DNA with given random generator
breed :: forall m. (PrimMonad m) => Gen (PrimState m) -> DNA.DNA -> DNA.DNA -> m DNA.DNA
breed g a b = do
    rands <- uniformVector g (G.length a) :: m (U.Vector Word8)
    return $ G.map choose (G.zip3 a b rands)
    where
        choose :: (Word8, Word8, Word8) -> Word8
        choose (geneA, geneB, r) = mated where
            ar = r .&. 0xAA
            car = complement r .&. 0xAA
            br = r .&. 0x55
            cbr = complement r .&. 0x55
            ac = (ar .&. geneA) .|. (car .&. unsafeShiftL geneA 1)
            bc = (br .&. geneB) .|. (cbr .&. unsafeShiftR geneB 1)
            mated = ac .|. bc


-- |
-- helper
findIndices :: (G.Vector v a, G.Vector v Int) => (a -> Bool) -> v a -> v Int
findIndices f v = G.unfoldr findNext 0 where
    findNext i = if
        | i >= G.length v -> Nothing
        | f (v G.! i) -> Just (i, i+1)
        | otherwise -> findNext (i+1)

-- |
-- helper
uniformVectorR :: (PrimMonad m, Variate a, G.Vector v a)
             => (a, a) -> Gen (PrimState m) -> Int -> m (v a)
uniformVectorR range gen n = G.replicateM n (uniformR range gen)
{-# INLINE uniformVectorR #-}

-- |
-- chance is chance of one bit mutating per byte
mutate :: forall m. (PrimMonad m) => Float -> Gen (PrimState m) -> DNA.DNA -> m DNA.DNA
mutate chance g dna = do
    rands <- uniformVector g (G.length dna)
    let
        indices = findIndices (< chance) rands
        mutateBit x index = unsafeShiftL 0x01 index `xor` x
    bitRands <- uniformVectorR (0, 7) g (G.length indices)
    return $ U.accumulate_ mutateBit dna indices bitRands


breedAndMutate :: (PrimMonad m) => Float -> Gen (PrimState m) -> DNA.DNA -> DNA.DNA -> m DNA.DNA
breedAndMutate chance g a b = do
    dna' <- breed g a b
    mutate chance g dna'

breedAndSelectPool :: forall m. (PrimMonad m) =>
    (DNA.DNA -> Float) -- ^ test function
    -> Float -- ^ mutation chance
    -> Gen (PrimState m) -- ^ random generator
    -> (Int, Int) -- ^ size, winner
    -> [DNA.DNA] -- ^ parent pool
    -> m [DNA.DNA] -- ^ best children and new generator
breedAndSelectPool testfn chance g (size, winners) dnas = do
    let inputs = length dnas
    moms <- uniformVectorR (0,inputs-1) g size :: m (V.Vector Int)
    dads <- uniformVectorR (0,inputs-1) g size
    children <- G.mapM (\(mom, dad) -> breedAndMutate chance g (dnas !! mom) (dnas !! dad)) (G.zip moms dads)
    let
        r = take winners $ sortBy (comparing testfn) (G.toList children)
        -- r' = head r
    return r
