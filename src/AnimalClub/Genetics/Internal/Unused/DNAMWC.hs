{-|
Module      : DNAMWC
Description : DNA methods implemented using mwc-random
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

This module has the same functionality as AnimalClub.Genetics.DNA except implemented using random-mwc
The signatures are adjusted accordingly
MWC is slower than StdGen and in this case we do not need the better random numbers
Since the implementation is non-trivial and this may be useful someday, the code is left here for reference


Use below for performance testing:

---------------------------
-- performance testing code
---------------------------

import qualified System.Random.MWC as MWC
import qualified AnimalClub.Genetics.DNAMWC as MWC
...
mwcvsstd :: IO ()
mwcvsstd = do
  let
    lns = [100, 1000, 10000]
  g <- MWC.create
  stdg <- getStdGen
  dnas <- mapM (MWC.makeRandDNA g) lns
  defaultMain [
    bgroup "MWC create" $ map (\l -> bench (show l) $ nfIO (MWC.makeRandDNA g l)) lns
    ,bgroup "StdGen create" $ map (\l -> bench (show l) $ nf (makeRandDNA stdg) l) lns
    ,bgroup "MWC breed" $ map (\(l, dna) -> bench (show l) $ nfIO (MWC.breed g dna dna)) (zip lns dnas)
    ,bgroup "StdGen breed" $ map (\(l, dna) -> bench (show l) $ nf (breed stdg dna) dna) (zip lns dnas)
    ,bgroup "MWC mutate 0.1" $ map (\(l, dna) -> bench (show l) $ nfIO (MWC.mutate 0.1 g dna)) (zip lns dnas)
    ,bgroup "StdGen mutate 0.1" $ map (\(l, dna) -> bench (show l) $ nf (mutate 0.1 stdg) dna) (zip lns dnas)
    ]


-}

module AnimalClub.Genetics.Internal.Unused.DNAMWC (
  makeRandDNA,
  breed,
  mutate,
  breedAndMutate,
  breedAndSelectPool
) where

import qualified AnimalClub.Genetics.DNA as DNA
import           Data.Bits
import           Data.List               (sortBy)
import           Data.Ord                (comparing)
import qualified Data.Vector             as V
import qualified Data.Vector.Generic     as G
import qualified Data.Vector.Storable    as S
import           Data.Word
import           System.Random.MWC

import           Control.Monad.Primitive

-- | create random DNA with given dnaLength
-- length must be multiple of 4
makeRandDNA :: (PrimMonad m) => Gen (PrimState m) -> Int -> m (DNA.DNA n)
makeRandDNA g c = uniformVector g c

-- | breed 2 DNA with given random generator
breed :: forall m. (PrimMonad m) => Gen (PrimState m) -> (DNA.DNA n) -> (DNA.DNA n) -> m (DNA.DNA n)
breed g a b = do
  rands <- uniformVector g (G.length a) :: m (S.Vector Word8)
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
mutate :: forall m. (PrimMonad m) => Float -> Gen (PrimState m) -> (DNA.DNA n) -> m (DNA.DNA n)
mutate chance g dna = do
  rands <- uniformVectorR (0, 1.0) g (G.length dna)
  let
    indices = findIndices (< chance) rands
    mutateBit x index = unsafeShiftL 0x01 index `xor` x
  bitRands <- uniformVectorR (0, 7) g (G.length indices)
  return $ S.accumulate_ mutateBit dna indices bitRands


breedAndMutate :: (PrimMonad m) => Float -> Gen (PrimState m) -> (DNA.DNA n) -> (DNA.DNA n) -> m (DNA.DNA n)
breedAndMutate chance g a b = do
  dna' <- breed g a b
  mutate chance g dna'

breedAndSelectPool :: forall m. (PrimMonad m) =>
  ((DNA.DNA n) -> Float) -- ^ test function
  -> Float -- ^ mutation chance
  -> Gen (PrimState m) -- ^ random generator
  -> (Int, Int) -- ^ size, winner
  -> [(DNA.DNA n)] -- ^ parent pool
  -> m [(DNA.DNA n)] -- ^ best children and new generator
breedAndSelectPool testfn chance g (size, winners) dnas = do
  let inputs = length dnas
  moms <- uniformVectorR (0,inputs-1) g size :: m (V.Vector Int)
  dads <- uniformVectorR (0,inputs-1) g size
  children <- G.mapM (\(mom, dad) -> breedAndMutate chance g (dnas !! mom) (dnas !! dad)) (G.zip moms dads)
  let
    r = take winners $ sortBy (comparing testfn) (G.toList children)
    -- r' = head r
  return r
