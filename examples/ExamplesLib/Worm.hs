{-|
Module      : ExamplesLib.Worm
Description : Example worm
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

-}

--{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
--{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module ExamplesLib.Worm (
 worm,
 wormGenome,
 testWorm,
 breedAndSelectWormPool
) where

import           Relude                      hiding (head, last)
import           Relude.Unsafe               (head, last, (!!))

import           AnimalClub.Animals
import           AnimalClub.Genetics
import           AnimalClub.Skellygen        hiding (distance)
import           AnimalClub.Skellygen.Linear
import           AnimalClub.Skellygen.TRS



import           Control.Monad               (forM_)
import           Data.List                   (mapAccumL, sortBy)
import qualified Data.Map                    as Map
import           Data.Ord                    (comparing)
import           System.Random

import qualified Debug.Trace                 as Debug


wormNode :: (AnimalFloat a) => Int -> AnimalNode a
wormNode 0 = mans (show 0) (Rel $ V3 0.5 0 0) (Rel 1) []
wormNode n = mans (show n) (Rel $ V3 0.5 0 0) (Rel 1) [wormNode (n-1)]

worm :: (AnimalFloat a) => Int -> AnimalNode a
worm segs = asPhantom $ mans (show (segs-1)) (Rel 0) (Abs 0.1) [wormNode (segs-1)]

wormGenome' :: (RandomGen g) => Int -> Int -> Genotype g [AnimalExp Float [Float]] ()
wormGenome' segs dnaPerSeg = do forM_ [0..(segs-1)] wormSeg where
  --dnaPerSegOver4 = dnaPerSeg `div` 4
  dnaPerSegOver2 = dnaPerSeg `div` 2
  wormSeg i = do
    --gbPush $ Gene (dnaPerSeg*i) dnaPerSeg
    usingGene (Gene (dnaPerSeg*i) dnaPerSegOver2) $ do
      x <- gbSumRange (-1.0, 6.0) -- using gbTypical here doesn't work very well for some reason :(
      --x <- gbSumRange (0.1, 4.5)
      tellSkellyFunc (WithBoneId (BoneId (show i) []) (Thickness x)) --[x*0.5+0.75]
    --gbPush $ Gene (dnaPerSeg*i + dnaPerSegOver4*2) (dnaPerSegOver4*2)
    usingGene (Gene (dnaPerSeg*i + dnaPerSegOver2) (dnaPerSegOver2)) $ do
      orients <- gbRandomRanges (replicate 3 (-1.5,1.5))
      tellSkellyFunc (WithBoneId (BoneId (show i) []) (addValuesToBoneMethod defOrientation orients))

-- | generate the genome of a worm
wormGenome ::
 Int -- ^ number of segments
 -> Int -- ^ DNA length of each segment
 -> Genome StdGen [AnimalExp Float [Float]]
wormGenome segs dnaPerSeg = Genome (segs*dnaPerSeg) (wormGenome' segs dnaPerSeg) (mkStdGen 0)


-- | testWorm tests the worm against the ideal worm
-- the definition of ideal worms changes as cultural expectations evolve
-- a brief history of this evolution is available through `git blame`
testWorm ::
 Int -- ^ number of segments
 -> AnimalPropertyMap Float -- ^ the worm's AnimalPropertyMap
 -> Float
testWorm segs props = score where
  --desiredThick i =  (fromIntegral i / fromIntegral segs) * 3 + 0.5
  desiredThick i =  (cos ((fromIntegral i / fromIntegral segs) * pi * 2 * 2)*3 + 1.2)
  --desiredOrient _ = fromEulerXYZ (V3 (pi/20) (pi/3) 0.0)
  desiredOrient _ = fromEulerXYZ (V3 0 (pi/6) 0)
  name i = BoneId (show i) []
  -- find the segment in the worm's AnimalPropertyMap
  prop i = Map.findWithDefault (error $ "could not find " <> show (name i)) (name i) props
  thick i = _skinParams $ prop i
  orient i = _orientation $ prop i
  off i = (thick i - desiredThick i) + 5*(distance (orient i) (desiredOrient i))
  score = sqrt $ sum [off x * off x | x <- [0..(segs-1)]]

-- | breedAndSelectWormPool breeds worms in a pool targetting the ideal form
-- TODO use breedAndSelectPool in DNA <-- I can't remember what this means anymore
breedAndSelectWormPool :: (RandomGen g) =>
  (AnimalPropertyMap Float -> Float) -- ^ test function
  -- TODO consider packing this into its own type since they are used together a lot
  -> ([BoneId], Genome StdGen [AnimalExp Float [Float]]) -- ^ worm
  -> Float -- ^ mutation chance
  -> g -- ^ random generator
  -> (Int, Int) -- ^ size, # winners to go to next generation
  -> [DNA] -- ^ parent pool
  -> ([DNA], g) -- ^ best children and new generator
breedAndSelectWormPool testfn (bids, genome) mChance g (size, winners) dnas = Debug.trace (show (testfn (wormProps r'))) (r, outg) where
  inputs = length dnas
  (g', g'') = split g
  moms = randomRs (0,inputs-1) g'
  dads = randomRs (0,inputs-1) g''
  parents = take size $ zip moms dads
  (outg, worms) = mapAccumL (\acc_g x -> (snd (next acc_g), breedAndMutate mChance acc_g (dnas !! fst x ) (dnas !! snd x))) g parents
  wormProps dna' = generateAnimalProperties bids $ evalGenome genome dna'
  r = take winners $ sortBy (comparing (\x -> testfn (wormProps x))) worms
  r' = head r

{-
-- | delete me
breedAndSelectSingle :: (RandomGen g) => g -> Int -> DNA -> (DNA, g)
breedAndSelectSingle breedg n dna = Debug.trace (show $ testWorm (wormProps r)) (r, outbreedg) where
  (outbreedg, worms) = mapAccumL (\g' _ -> (snd (next g'), breed g' dna dna)) breedg [0..(n-1)]
  wormProps dna' = generateAnimalProperties $ evalGenome wormGenome dna'
  r = maximumBy (comparing (\x -> (-1) * testWorm (wormProps x))) worms-}
