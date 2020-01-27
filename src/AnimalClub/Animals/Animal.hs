{-|
Module      : Animal
Description : Binds Genetics and Skellygen together
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental
-}


module AnimalClub.Animals.Animal (
    asPhantom,
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
import           AnimalClub.Skellygen.Linear
import           AnimalClub.Skellygen.TRS

import qualified Data.ByteString.Lazy        as B
import           Data.Maybe                  (catMaybes)
import qualified Data.Text                   as T

import           Control.Monad.Writer        (tell)

--import Control.Exception (assert)
--import Debug.Trace (trace)

-- | set the AnimalNode as the root node
asPhantom :: AnimalNode a -> AnimalNode a
asPhantom an = an { _isPhantom = True }

-- | helper method for making AnimalNode
mans ::
  (AnimalFloat a)
  => T.Text -- ^ name
  -> AbsOrRel (V3 a) -- ^ position
  -> AbsOrRel a -- ^ thickness
  -> [AnimalNode a] -- ^ children
  -> AnimalNode a
mans n p t c = manf n [] p t c

-- | helper method for making AnimalNode
manf ::
  (AnimalFloat a)
  => T.Text -- ^ name
  -> [BoneFlag]
  -> AbsOrRel (V3 a) -- ^ position
  -> AbsOrRel a -- ^ thickness
  -> [AnimalNode a] -- ^ children
  -> AnimalNode a
manf n f p t c = manbt n f Same p t c

-- | helper method for making AnimalNode
manbt ::
  (AnimalFloat a)
  => T.Text -- ^ name
  -> [BoneFlag]
  -> BoneTrans a
  -> AbsOrRel (V3 a) -- ^ position
  -> AbsOrRel a -- ^ thickness
  -> [AnimalNode a] -- ^ children
  -> AnimalNode a
manbt n f bt p t c = AnimalNode (BoneId n f) bt p t False c


-- TODO add something like ExpSkellyFuncPrioritized
-- FUTURE is it possible to parmeterize AnimalExp to allow users to pass in their own data types
-- e.g. 'ExpUser T.Text a' or something like that... (ideally a list of types...)
-- | various output expression of an animal genotype
data AnimalExp a =
    ExpBytes T.Text B.ByteString
    | ExpFloats T.Text [a]
    | ExpSkellyFunc (SkellyFunc a) deriving (Show)

-- | pull out just the SkellyFuncs
parseSkellyFuncs :: [AnimalExp a] -> [SkellyFunc a]
parseSkellyFuncs = catMaybes . fmap (\case
    ExpSkellyFunc sf -> Just sf
    _ -> Nothing)

-- | helper monad for writing out SkellyFuncs
tellSkellyFunc :: (Monad m) => SkellyFunc a -> GenotypeT g [AnimalExp a] m ()
tellSkellyFunc sf = tell [ExpSkellyFunc sf]

-- | generates 'AnimalPropertyMap' from list of 'BoneId's and 'AnimalExp's
generateAnimalProperties ::
    (AnimalFloat a)
    => [BoneId] -- ^ list of all bones
    -> [AnimalExp a] -- ^ list of properties
    -> AnimalPropertyMap a-- ^ output accumulated map of properties.
generateAnimalProperties bids = generateAnimalProperties_ bids . map (\sf -> PrioritizedSkellyFunc (0,sf)) . parseSkellyFuncs


-- breedAndSelectPoolProperty :: (RandomGen g) =>
