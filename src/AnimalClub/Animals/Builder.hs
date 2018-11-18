{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

{-|
Module      : Builder
Description : Binds Genetics and Skellygen together
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

-}

module AnimalClub.Animals.Builder (
    AutoGeneMethod(..),
    makeGenomeFromPropertiesSimple,

    -- WIP
    ATree,
    DepFunc,
) where

import AnimalClub.Animals.Animal
import AnimalClub.Genetics
import AnimalClub.Skellygen

import qualified Data.Text as T
import Data.List (mapAccumL)
import Control.DeepSeq
import GHC.Generics (Generic)
import System.Random
import Lens.Micro.Platform
import Control.Monad
import Control.Monad.Writer (tell)

--import Debug.Trace (trace)


data DepFunc = Linear Float | Mutate Float

data ATree = ATree ((Either T.Text SkellyFunc, AutoGeneMethod), [(DepFunc, ATree)])










data AutoGeneMethod =
    Normal (Float, Float) Int -- ^ normal distribution: (min, max) num_outputs
    deriving (Generic, NFData)


-- | returns number of floats this genotype creates
autoGeneCount :: AutoGeneMethod -> Int
autoGeneCount (Normal _ x) = x

-- | returns relative amount of DNA this genotype should take up
-- note, this is NOT the same as how many float values need to be produced
autoGeneSize :: AutoGeneMethod -> Int
autoGeneSize (Normal _ x) = x

-- | automatically create genome from given properties
-- this version does no overlap. All properties are independent
makeGenomeFromPropertiesSimple ::
    Int -- ^ DNA length (vector length / 4)
    -> [(T.Text, AutoGeneMethod)] -- ^ other properties
    -> [(SkellyFunc, AutoGeneMethod)] -- ^ skellygen properties
    -> Genome StdGen [AnimalExp] -- ^ output genome
makeGenomeFromPropertiesSimple dnasz ops sfps = Genome dnasz geneBuilder (mkStdGen 0) where
    -- combine other properties and skellygen properties into a single list
    aps = map (over _1 Left) ops ++ map (over _1 Right) sfps
    -- sum the weights and track the totals (total weights, incremental weights)
    --(total, withTotals) :: (Int, (Either T.Text SkellyFunc, AutoGenoMethod, Int))
    (total, withTotals) = mapAccumL (\acc (x, agm) -> (acc + autoGeneSize agm, (x, agm, acc))) 0 aps
    geneBuilder = forM_ withTotals $ \(x, agm, startWeight) -> do
        let
            -- use accumulated weights and total to determine start and length of the gene
            gtsize = dnasz * (autoGeneSize agm) `div` total
            start = dnasz * startWeight `div` total
        -- and build the genotype for the gene as defined by AutoGeneMethod
        --trace (show start ++ " : " ++ show gtsize) $ usingGene (Gene start gtsize) $ do
        usingGene (Gene start gtsize) $ do
            case agm of
                Normal range cnt -> do
                    vals <- forM [0..(cnt-1)] $ \n -> do
                        --trace ((show $ n * gtsize `div` cnt) ++ " ! " ++ (show $ gtsize `div` cnt)) $  gbPush (Gene (n * gtsize `div` cnt) (gtsize `div` cnt))
                        usingGene (Gene (n * gtsize `div` cnt) (gtsize `div` cnt)) $ gbSumRange range
                    case x of
                        Right sf -> tell [ExpSkellyFunc sf vals]
                        Left t -> tell [ExpFloats t vals]
