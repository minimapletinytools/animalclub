{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{-|
Module      : Animal
Description : Binds Genetics and Skellygen together
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

-}

module AnimalClub.Animals.Animal (
    asRoot,
    mans, manf, manbt,
    AnimalExp(..),
    parseSkellyFuncs,
    tellSkellyFunc,
    Genome(..),
    evalGenome,
    generateAnimalProperties
) where

import           AnimalClub.Genetics
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Math.Hierarchical
import           Linear.V3                              (V3)

import qualified Data.ByteString.Lazy                   as B
import           Data.Maybe                             (catMaybes)
import qualified Data.Text                              as T

import           Control.DeepSeq
import           Control.Monad.Writer                   (tell)
import           GHC.Generics                           (Generic)

--import Control.Exception (assert)
--import Debug.Trace (trace)

-- | set the AnimalNode as the root node
asRoot :: AnimalNode -> AnimalNode
asRoot an = an { _isRoot = True }

-- | helper method for making AnimalNode
mans ::
  T.Text -- ^ name
  -> AbsOrRel (V3 Float) -- ^ position
  -> AbsOrRel Float -- ^ thickness
  -> [AnimalNode] -- ^ children
  -> AnimalNode
mans n p t c = manf n [] p t c

-- | helper method for making AnimalNode
manf ::
  T.Text -- ^ name
  -> [BoneFlag]
  -> AbsOrRel (V3 Float) -- ^ position
  -> AbsOrRel Float -- ^ thickness
  -> [AnimalNode] -- ^ children
  -> AnimalNode
manf n f p t c = manbt n f Same p t c

-- | helper method for making AnimalNode
manbt ::
  T.Text -- ^ name
  -> [BoneFlag]
  -> BoneTrans
  -> AbsOrRel (V3 Float) -- ^ position
  -> AbsOrRel Float -- ^ thickness
  -> [AnimalNode] -- ^ children
  -> AnimalNode
manbt n f bt p t c = AnimalNode (BoneWT (BoneId n f) bt) p t False c



-- TODO fix the SkellyFunc part, all internal stuff should be type safe :O
-- TODO is it possible to parmeterize AnimalExp to allow users to pass in their own data types
-- e.g. 'ExpUser T.Text a' or something like that... (ideally a list of types...)
-- | various output expression of an animal genotype
data AnimalExp =
    ExpBytes T.Text B.ByteString
    | ExpFloats T.Text [Float] -- probably just use any type instead of [Float]?
    | ExpSkellyFunc SkellyFunc deriving (Show)

-- | pull out just the SkellyFuncs
parseSkellyFuncs :: [AnimalExp] -> [SkellyFunc]
parseSkellyFuncs = catMaybes . fmap (\case
    ExpSkellyFunc sf -> Just sf
    _ -> Nothing)

-- | helper monad for writing out SkellyFuncs
tellSkellyFunc :: (Monad m) => SkellyFunc -> GenotypeT g [AnimalExp] m ()
tellSkellyFunc sf = tell [ExpSkellyFunc sf]

generateAnimalProperties ::
    [BoneId] -- ^ list of all bones
    -> [AnimalExp] -- ^ list of properties
    -> AnimalPropertyMap -- ^ output accumulated map of properties. EnumBone' property will override AllBone' property
generateAnimalProperties bids = generateAnimalProperties_ bids . map (\sf -> PrioritizedSkellyFunc (0,sf)) . parseSkellyFuncs


-- breedAndSelectPoolProperty :: (RandomGen g) =>
