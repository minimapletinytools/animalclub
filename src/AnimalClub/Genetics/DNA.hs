{-|
Module      : DNA
Description : Functions and types pertaining to DNA
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

-}

module AnimalClub.Genetics.DNA (
  -- * Types
  DNA,
  -- * Functions
  makeZeroDNA,
  makeRandDNA,
  dnaLength,
  dnaSum,
  dnaBitCount,
  breed,
  mutate,
  mutateOld,
  breedAndMutate,

  breedAndSelectPool,
  FitnessFunc

) where

import           Data.Bits
import           Data.List              (mapAccumL, sortBy)
import           Data.Ord               (comparing)
import qualified Data.Vector.Generic    as G
import qualified Data.Vector.Storable   as V
import           Data.Word
import           Foreign.Storable.Tuple ()
import           System.Random

-- we don't use repa because repa uses Vector.Unboxed as its underlying type which is not tightly packed

-- TODO replace with UArray Word32 Bool D: it's tightly packed like you want... fml
--type DNA = A.UArray Bool

-- | Allele4 is 4 single gene pairs (allele represented as a Word8
-- not really necessary type synonym tbh
type Allele4 = Word8

-- | DNA host genetic information for all gene transformations in this module
-- DNA is an array of 'Allele4'
type DNA = V.Vector Allele4

-- | number of Allele4 in DNA
-- that is to say, length in in 8-bit intervals
dnaLength :: (Num a) => DNA -> a
dnaLength = fromIntegral . V.length

-- | sum of all bits in DNA
dnaSum :: (Num a) => DNA -> a
dnaSum = V.foldl' (\acc x -> acc + fromIntegral (popCount x)) 0

-- | returns an 8 length array that counts occurrence of each bit
dnaBitCount :: DNA -> V.Vector Int
dnaBitCount = V.foldl' f (V.replicate 8 0) where
  f acc x = V.imap (\i a -> if ((unsafeShiftL 0x01 i) .&. x) /= 0 then a+1 else a) acc

-- | create DNA of all 0s with given dnaLength
makeZeroDNA :: Int -> DNA
makeZeroDNA c = V.generate c (const 0)

-- | create random DNA with given dnaLength
makeRandDNA :: (RandomGen g) => g -> Int -> DNA
makeRandDNA g c = V.unfoldrN c (Just . random) g

-- | breed 2 DNAs with given random generator
-- TODO write a version that takes a seed instead and uses the faster RNG maybe?
breed :: (RandomGen g) => g -> DNA -> DNA -> DNA
breed g a b = V.map choose (G.zip3 a b rands) where
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



-- |
-- helper
findIndices :: (G.Vector v a, G.Vector v Int) => (a -> Bool) -> v a -> v Int
findIndices f v = G.unfoldr findNext 0 where
  findNext i = if
    | i >= G.length v -> Nothing
    | f (v G.! i) -> Just (i, i+1)
    | otherwise -> findNext (i+1)

-- |
-- chance is chance of one bit mutating per byte
mutate :: (RandomGen g) => Float -> g -> DNA -> DNA
mutate chance g dna = V.accumulate_ mutateBit dna indices bitRands where
  rands = V.fromList . take (V.length dna) . randomRs (0, 1.0) $ g
  indices = findIndices (< chance) rands
  bitRands = V.fromList . take (V.length indices) . randomRs (0,7) $ g
  mutateBit x index = unsafeShiftL 0x01 index `xor` x

-- | old inefficient implementation, left for peformance testing reasons
-- chance is chance of one bit mutating per byte
mutateOld :: (RandomGen g) => Float -> g -> DNA -> DNA
mutateOld chance g dna = V.zipWith zipFunc rands dna where
  -- TODO running random twice here is very inefficient
  -- you really want to mapAccumL a single generator over the dna
  -- and only evaluate a second time when there is a mutation
  rands = V.fromList . take (V.length dna) . randoms $ g
  zipFunc gx x = r where
    mutateBit g' x' = unsafeShiftL 0x01 (fst $ randomR (0,7) g') `xor` x'
    (c, gx') = randomR (0,1.0) (mkStdGen gx)
    r = if c < chance then mutateBit gx' x else x

breedAndMutate :: (RandomGen g) => Float -> g -> DNA -> DNA -> DNA
breedAndMutate chance g a b = dna where
  (g',g'') = split g
  dna' = breed g' a b
  dna = mutate chance g'' dna'

-- TODO separate this out to a different file
-- e.g. Fitness.hs or something
type FitnessFunc = DNA -> Float
breedAndSelectPool :: (RandomGen g) =>
  FitnessFunc -- ^ test function
  -> Float -- ^ mutation chance
  -> g -- ^ random generator
  -> (Int, Int) -- ^ size, winner
  -> [DNA] -- ^ parent pool
  -> ([DNA], g) -- ^ best children and new generator
breedAndSelectPool testfn mChance g (size, winners) dnas = (r, outg) where
  inputs = length dnas
  (g', g'') = split g
  moms = randomRs (0,inputs-1) g'
  dads = randomRs (0,inputs-1) g''
  parents = take size $ zip moms dads
  (outg, worms) = mapAccumL (\acc_g x -> (snd (next acc_g), breedAndMutate mChance acc_g (dnas !! fst x ) (dnas !! snd x))) g parents
  r = take winners $ sortBy (comparing testfn) worms
  --r' = head r



{-

I can't remember why I left this here, pretty sure it's safe to delete


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
