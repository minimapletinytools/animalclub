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
    AnimalFloats,
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
import Data.List (foldl', mapAccumL)
import Control.DeepSeq
import GHC.Generics (Generic)
import System.Random
import Lens.Micro.Platform
import Control.Monad
import Control.Monad.Writer (tell)
--import Control.Exception (assert)

import Debug.Trace (trace)

-- TODO change to Word32 or Bytestring
type AnimalFloats = [(Either T.Text SkellyFunc, [Float])]

-- | various output expression of an animal genotype
data AnimalExp =
    ExpBytes T.Text [Word32]
    | ExpFloats T.Text [Float]
    | ExpSkellyFunc T.Text SkellyFunc [Float]

tellBoneFunc :: (Monad m) => BoneName' -> BoneMethod -> [Float] -> GenotypeT g AnimalFloats m ()
tellBoneFunc bn bm v = tell [(Right (SkellyFunc bn bm), v)]

-- | DNA length, builder, random gen to seed builder
data Genome g w = Genome Int (Genotype g w ()) g

-- | evalute the genome and obtain its output
evalGenome :: (RandomGen g, Monoid w) => Genome g w -> DNA -> w
evalGenome (Genome _ gb g) dna = evalGeneBuilder gb (dna,[]) g

generateAnimalProperties ::
    AnimalFloats -- ^ list of properties
    -> AnimalPropertyMap -- ^ output accumulated map of properties. EnumBone' property will override AllBone' property
generateAnimalProperties afs = generateAnimalProperties_ parsedafs where
    parsedafs = foldl' foldfunc [] afs
    foldfunc xs (sf, v) = case sf of
        Left _ -> xs -- throw out non SkellyFunc values
        Right x -> (x,v):xs


-- breedAndSelectPoolProperty :: (RandomGen g) =>
