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
import Data.List (foldl', mapAccumL)
import AnimalClub.Genetics
import AnimalClub.Skellygen
import System.Random
import Control.Lens
import Control.Monad
import Control.Monad.Writer (tell)
--import Control.Exception (assert)

type AnimalFloats = [(Either T.Text SkellyFunc, [Float])]

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

-- TODO move to genetics
data AutoGeneMethod =
    Normal (Float, Float) Int


-- | returns number of floats this genotype creates
autoGeneCount :: AutoGeneMethod -> Int
autoGeneCount (Normal _ x) = x

-- | returns relative amount of DNA this genotype should take up
-- note, this is NOT the same as how many float values need to be produced
autoGeneSize :: AutoGeneMethod -> Int
autoGeneSize (Normal _ x) = x

-- | automatically create genome from given properties
-- this version does no overlap. All properties are independent
-- UNSTESTED
makeGenomeFromPropertiesSimple ::
    Int -- ^ DNA length (vector length / 4)
    -> [(T.Text, AutoGeneMethod)] -- ^ other properties
    -> [(SkellyFunc, AutoGeneMethod)] -- ^ skellygen properties
    -> Genome StdGen AnimalFloats -- ^ output genome
makeGenomeFromPropertiesSimple dnasz ops sfps = Genome dnasz geneBuilder (mkStdGen 0) where
    aps = map (over _1 Left) ops ++ map (over _1 Right) sfps
    (total, withTotals) = mapAccumL (\acc (x,ag) -> (acc+autoGeneSize ag,(x, ag, acc))) 0 aps
    geneBuilder = forM_ withTotals $ \(x, ag, start) -> do
        let
            gtsize = (dnasz) * (autoGeneSize ag) `div` total
        gbPush (Gene start (start+gtsize))
        case ag of
            Normal range cnt -> forM_ [0..(cnt-1)] $ \n -> do
                gbPush (Gene (n * gtsize) (gtsize `div` cnt))
                val <- gbTypical range
                tell [(x, [val])]
                gbPop
        gbPop
