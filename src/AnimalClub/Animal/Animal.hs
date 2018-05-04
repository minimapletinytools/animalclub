{-|
Module      : Animal
Description : Binds Genetics and Skellygen together
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

-}

module AnimalClub.Animal.Animal (
    tellBoneFunc,
    Genome(..),
    evalGenome
) where

import qualified Data.Text as T
import Data.Monoid (Monoid)
import AnimalClub.Genetics
import AnimalClub.Skellygen.AnimalNode
import AnimalClub.Skellygen.AnimalProperty
import System.Random (RandomGen)
import Control.Monad
--import Control.Exception (assert)

tellBoneFunc :: (RandomGen g, Monad m) => BoneName' -> BoneMethod -> [Float] -> FastGeneBuilderT g NamedFloats m ()
tellBoneFunc bn bm = tellGenes (T.pack $ show (SkellyFunc bn bm))

-- | DNA length, builder, random gen to seed builder
data Genome g w = Genome Int (FastGeneBuilder g w ()) g

-- | evalute the genome and obtain its output
evalGenome :: (RandomGen g, Monoid w) => Genome g w -> DNA -> w
evalGenome (Genome _ gb g) dna = evalGeneBuilder gb (dna,[]) g

--makeGenomeFromProperties ::
