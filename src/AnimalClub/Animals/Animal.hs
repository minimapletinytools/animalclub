{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

{-|
Module      : Animal
Description : Binds Genetics and Skellygen together
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

-}

module AnimalClub.Animals.Animal (
    AnimalExp(..),
    parseSkellyFuncs,
    tellBoneFunc,
    Genome(..),
    evalGenome,
    generateAnimalProperties,
) where

import AnimalClub.Genetics
import AnimalClub.Skellygen

import qualified Data.Text as T
import           Data.Word
import Data.Monoid (Monoid)
import Data.Maybe (catMaybes)
import Control.DeepSeq
import GHC.Generics (Generic)
import System.Random
import Control.Monad.Writer (tell)

--import Control.Exception (assert)
--import Debug.Trace (trace)

-- | various output expression of an animal genotype
data AnimalExp =
    ExpBytes T.Text [Word32]
    | ExpFloats T.Text [Float]
    | ExpSkellyFunc SkellyFunc [Float] deriving (Generic, NFData, Show)

-- | pull out just the SkellyFuncs
parseSkellyFuncs :: [AnimalExp] -> [(SkellyFunc, [Float])]
parseSkellyFuncs = catMaybes . fmap (\case
    ExpSkellyFunc sf f -> Just (sf,f)
    _ -> Nothing)

tellBoneFunc :: (Monad m) => BoneName' -> BoneMethod -> [Float] -> GenotypeT g [AnimalExp] m ()
tellBoneFunc bn bm v = tell [(ExpSkellyFunc (SkellyFunc bn bm) v)]

-- | DNA length, builder, random gen to seed builder
data Genome g w = Genome Int (Genotype g w ()) g

-- | evalute the genome and obtain its output
evalGenome :: (RandomGen g, Monoid w) => Genome g w -> DNA -> w
evalGenome (Genome _ gb g) dna = evalGeneBuilder gb (dna,[]) g

generateAnimalProperties ::
    [AnimalExp] -- ^ list of properties
    -> AnimalPropertyMap -- ^ output accumulated map of properties. EnumBone' property will override AllBone' property
generateAnimalProperties = generateAnimalProperties_ . parseSkellyFuncs


-- breedAndSelectPoolProperty :: (RandomGen g) =>
