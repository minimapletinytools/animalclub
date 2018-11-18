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
    tellSkellyFunc,
    Genome(..),
    evalGenome,
    generateAnimalProperties,
) where

import AnimalClub.Genetics
import AnimalClub.Skellygen

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Maybe (catMaybes)

import Control.DeepSeq
import GHC.Generics (Generic)
import Control.Monad.Writer (tell)

--import Control.Exception (assert)
--import Debug.Trace (trace)

-- | various output expression of an animal genotype
data AnimalExp =
    ExpBytes T.Text B.ByteString
    | ExpFloats T.Text [Float]
    | ExpSkellyFunc SkellyFunc [Float] deriving (Generic, NFData, Show)

-- | pull out just the SkellyFuncs
parseSkellyFuncs :: [AnimalExp] -> [(SkellyFunc, [Float])]
parseSkellyFuncs = catMaybes . fmap (\case
    ExpSkellyFunc sf f -> Just (sf,f)
    _ -> Nothing)

-- | helper monad for writing out SkellyFuncs
tellSkellyFunc :: (Monad m) => BoneName' -> BoneMethod -> [Float] -> GenotypeT g [AnimalExp] m ()
tellSkellyFunc bn bm v = tell [ExpSkellyFunc (SkellyFunc bn bm) v]

generateAnimalProperties ::
    [AnimalExp] -- ^ list of properties
    -> AnimalPropertyMap -- ^ output accumulated map of properties. EnumBone' property will override AllBone' property
generateAnimalProperties = generateAnimalProperties_ . parseSkellyFuncs


-- breedAndSelectPoolProperty :: (RandomGen g) =>
