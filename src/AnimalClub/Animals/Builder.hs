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
    makeGenomeFromPropertiesSimple
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


-- TODO move everything below to Cute/AutoGenotype.hs or osmetihng like that
data AutoGeneMethod =
    Normal (Float, Float) Int
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
-- UNSTESTED
makeGenomeFromPropertiesSimple ::
    Int -- ^ DNA length (vector length / 4)
    -> [(T.Text, AutoGeneMethod)] -- ^ other properties
    -> [(SkellyFunc, AutoGeneMethod)] -- ^ skellygen properties
    -> Genome StdGen [AnimalExp] -- ^ output genome
makeGenomeFromPropertiesSimple dnasz ops sfps = Genome dnasz geneBuilder (mkStdGen 0) where
    aps = map (over _1 Left) ops ++ map (over _1 Right) sfps
    --(total, withTotals) :: (Int, (Either T.Text SkellyFunc, AutoGenoMethod, Int)) -- (total weights, incremental weights)
    (total, withTotals) = mapAccumL (\acc (x,ag) -> (acc + autoGeneSize ag, (x, ag, acc))) 0 aps
    geneBuilder = forM_ withTotals $ \(x, ag, startWeight) -> do
        let
            gtsize = dnasz * (autoGeneSize ag) `div` total
            start = dnasz * startWeight `div` total
        --trace (show start ++ " : " ++ show gtsize) $ gbPush (Gene start gtsize)
        gbPush (Gene start gtsize)
        case ag of
            Normal range cnt -> do
                vals <- forM [0..(cnt-1)] $ \n -> do
                    --trace ((show $ n * gtsize `div` cnt) ++ " ! " ++ (show $ gtsize `div` cnt)) $  gbPush (Gene (n * gtsize `div` cnt) (gtsize `div` cnt))
                    gbPush (Gene (n * gtsize `div` cnt) (gtsize `div` cnt))
                    --val <- gbTypical range
                    val <- gbSumRange range
                    gbPop
                    return val
                case x of
                    Right sf -> tell [ExpSkellyFunc sf vals]
                    Left t -> tell [ExpFloats t vals]

        gbPop
