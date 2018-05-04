{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Worm (
worm, wormGenome, testWorm, breedAndSelectWormPool
) where

import AnimalClub.Animal.Animal
import AnimalClub.Genetics
import AnimalClub.Skellygen.AnimalScript
import AnimalClub.Skellygen.Hierarchical
import qualified AnimalClub.Skellygen.Quaternion as QH

import           Linear.V3
import qualified Linear.Metric as Metric

import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.List                       (sortBy, mapAccumL)
import           Data.Ord                        (comparing)
import Control.Monad (forM_)
import System.Random

import qualified Debug.Trace as Debug

textFromInt = T.pack . show

wormNode 0 = AnimalNode (Bone (textFromInt 0)) (Rel $ V3 0.5 0 0) (Rel 1) False []
wormNode n = AnimalNode (Bone (textFromInt n)) (Rel $ V3 0.5 0 0) (Rel 1) False [wormNode (n-1)]
worm segs = AnimalNode (Bone (textFromInt (segs-1))) (Rel 0) (Abs 0.1) True [wormNode (segs-2)]

wormGenome' :: (RandomGen g) => Int -> Int -> FastGeneBuilder g NamedFloats ()
wormGenome' segs dnaPerSeg = do forM_ [0..(segs-1)] wormSeg where
    dnaPerSegOver2 = dnaPerSeg `div` 2
    wormSeg i = do
        gbPush $ FastGenotype (dnaPerSeg*i) dnaPerSegOver2
        x <- gbNormalizedSum
        tellBoneFunc (Bone' (textFromInt i)) Thickness [x*8-3] --[x*0.5+0.75]
        gbPop
        gbPush $ FastGenotype (dnaPerSeg*i + dnaPerSegOver2) dnaPerSegOver2 -- overlaps with thickness why not :D
        orients <- gbRandomRanges (replicate 3 (-1.5,1.5))
        tellBoneFunc (Bone' (textFromInt i)) Orientation orients
        gbPop

wormGenome :: Int -> Int -> Genome StdGen NamedFloats
wormGenome segs dnaPerSeg = Genome (segs*dnaPerSeg) (wormGenome' segs dnaPerSeg) (mkStdGen 0)

testWorm :: Int -> AnimalPropertyMap -> Float
testWorm segs props = score where
    desiredThick i =  (fromIntegral i / fromIntegral segs) * 3 + 0.5
    --desiredThick i =  (sin ((fromIntegral i / fromIntegral wormSegs) * pi * 2)*0.5 + 1)
    desiredOrient _ = QH.fromEulerXYZ (V3 (pi/20) (pi/3) 0.0)
    name i = Bone' (textFromInt i)
    prop i = Map.findWithDefault (error $ "could not find " ++ show (name i)) (name i) props
    thick i = _skinParams $ prop i
    orient i = _orientation $ prop i
    off i = thick i - desiredThick i + 3*(Metric.distance (orient i) (desiredOrient i))
    score = sqrt $ sum [off x * off x | x <- [0..(segs-1)]]

-- |
-- TODO generalize this function and promote it to Genetics or to Animal
breedAndSelectWormPool :: (RandomGen g) =>
    (AnimalPropertyMap -> Float) -- ^ test function
    -> Genome StdGen NamedFloats -- ^ worm genome
    -> Float -- ^ mutation chance
    -> g -- ^ random generator
    -> (Int, Int) -- ^ size, winner
    -> [DNA] -- ^ parent pool
    -> ([DNA], g) -- ^ best children and new generator
breedAndSelectWormPool testfn genome mChance g (size, winners) dnas = Debug.trace (show (testfn (wormProps r'))) (r, outg) where
    inputs = length dnas
    (g', g'') = split g
    moms = randomRs (0,inputs-1) g'
    dads = randomRs (0,inputs-1) g''
    parents = take size $ zip moms dads
    (outg, worms) = mapAccumL (\acc_g x -> (snd (next acc_g), breedAndMutate mChance acc_g (dnas !! fst x ) (dnas !! snd x))) g parents
    -- TODO genome is getting reevaluated here every time this function is called for some resaon D:
    wormProps dna' = generateAnimalProperties $ evalGenome genome dna'
    r = take winners $ sortBy (comparing (\x -> testfn (wormProps x))) worms
    r' = head r

{-
-- | delete me
breedAndSelectSingle :: (RandomGen g) => g -> Int -> DNA -> (DNA, g)
breedAndSelectSingle breedg n dna = Debug.trace (show $ testWorm (wormProps r)) (r, outbreedg) where
    (outbreedg, worms) = mapAccumL (\g' _ -> (snd (next g'), breed g' dna dna)) breedg [0..(n-1)]
    wormProps dna' = generateAnimalProperties $ evalGenome wormGenome dna'
    r = maximumBy (comparing (\x -> (-1) * testWorm (wormProps x))) worms-}
