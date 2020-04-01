{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

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
  sfAutoGenome,
  makeGenomeFromPropertiesSimple,

  -- WIP
  ATree,
  DepFunc,
) where

import           Relude

import           AnimalClub.Animals.Animal
import           AnimalClub.Genetics
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.TRS

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Writer      (tell)
import           Data.List                 (mapAccumL)
import qualified Data.Text                 as T
import           GHC.Generics              (Generic)
import           Lens.Micro.Platform
import           System.Random

--import Debug.Trace (trace)


-- TODO figure out what this stuff is and finish it
data DepFunc a = Linear a | Mutate a
data ATree a = ATree ((Either T.Text (SkellyFunc a), AutoGeneMethod a), [(DepFunc a, ATree a)])


-- | defines parameters for automatically creating genes
-- genes express themselves as `AnimalExp a [a]`
data AutoGeneMethod a =
  Normal (a, a) Int -- ^ normal distribution: (min, max) num_outputs
  deriving (Generic, NFData)

-- | returns number of floats this genotype creates
autoGeneCount :: AutoGeneMethod a -> Int
autoGeneCount (Normal _ x) = x

-- | returns relative amount of DNA this genotype should take up
-- note, this is NOT the same as how many float values need to be produced, but it was convenient
autoGeneSize :: AutoGeneMethod a -> Int
autoGeneSize (Normal _ x) = x

sfAutoGenome ::
 (AnimalFloat a)
 => (BoneMethod a -> SkellyFunc a)
 -> ((a,a),(a,a),(a,a))
 -> [(SkellyFunc a, AutoGeneMethod a)]
sfAutoGenome bmf (l,t,o)= [
 (bmf defLength, Normal l 1)
 , (bmf defThickness, Normal t 1)
 , (bmf defOrientation, Normal o 3)
 ]

-- | automatically create genome from given lists of properties
-- this version does no overlap genes. All properties are independent
makeGenomeFromPropertiesSimple ::
  (AnimalFloat a)
  => Int -- ^ DNA length (vector length / 4)
  -> [(T.Text, AutoGeneMethod a)] -- ^ other properties
  -> [(SkellyFunc a, AutoGeneMethod a)] -- ^ skellygen properties, values in SkellyFunc are used as starting values
  -> Genome StdGen [AnimalExp a [a]] -- ^ output genome
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
            Left t   -> tell [ExpUser t vals]
            Right sf -> tell [ExpSkellyFunc (addValuesToSkellyFunc sf vals)]
