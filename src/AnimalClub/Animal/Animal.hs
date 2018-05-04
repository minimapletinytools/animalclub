{-|
Module      : Animal
Description : Binds Genetics and Skellygen together
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

-}

module AnimalClub.Animal.Animal (
    AnimalFloats,
    tellBoneFunc,
    Genome(..),
    evalGenome,
    generateAnimalProperties
) where

import qualified Data.Text as T
import Data.Monoid (Monoid)
import Data.Either
import Data.List (foldl')
import AnimalClub.Genetics
import AnimalClub.Skellygen.AnimalNode
import AnimalClub.Skellygen.AnimalProperty
import System.Random (RandomGen)
import Control.Monad
import Control.Monad.Writer (tell)
--import Control.Exception (assert)

type AnimalFloats = [(Either T.Text SkellyFunc, [Float])]

tellBoneFunc :: (RandomGen g, Monad m) => BoneName' -> BoneMethod -> [Float] -> FastGeneBuilderT g AnimalFloats m ()
tellBoneFunc bn bm v = tell [(Right (SkellyFunc bn bm), v)]

-- | DNA length, builder, random gen to seed builder
data Genome g w = Genome Int (FastGeneBuilder g w ()) g

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

--makeGenomeFromProperties ::
