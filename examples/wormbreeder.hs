{-

this example uses AnimalClub.Animals.Worm to breed a worm targetting some idea form

-}

--{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import           Relude                    hiding (head, last)
import           Relude.Unsafe             (head, last, (!!))

import           ExamplesLib.Worm

import           AnimalClub.Animals
import           AnimalClub.Genetics
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Mesh

import           Data.List                 (unfoldr)
import qualified Data.Text.IO              as T
import           System.Random
import           Text.Printf               (printf)


--import qualified Debug.Trace as Debug


main :: IO ()
main = do
  segs <- return 20
  dnaPerSeg <- return 25
  mutationChance <- return 0.002
  litterSize <- return 10
  selectionSize <- return 3
  generations <- return 1000
  printf "Breeding worms. segs: %i, dna per seg: %i, mutation: %f \n" segs dnaPerSeg mutationChance :: IO ()
  printf "litter: %i, selection: %i, generations: %i \n" litterSize selectionSize generations :: IO ()
  gen1 <- getStdGen
  let
    baseWorm = worm segs
    wormBones = makeBoneIdList baseWorm
    genome = (wormGenome segs dnaPerSeg)
    (_, gen2) = next gen1
    original = makeRandDNA gen1 (segs * dnaPerSeg)
    unfoldWormF (dnas, g) = Just $ (next_dnas, acc) where
      acc@(next_dnas, _) = breedAndSelectWormPool (testWorm segs) (wormBones, genome) mutationChance g (litterSize,selectionSize) dnas
    bestWorms = last $ take generations $ unfoldr unfoldWormF ([original], gen2)
    bestWorm = last bestWorms
    bestWormProps = generateAnimalProperties wormBones $ evalGenome genome bestWorm
    skelly = animalNodeToSkellyNodeWithProps bestWormProps (baseWorm)
  T.writeFile "wigglyworm.obj" . potatoMeshToObj . generatePotatoMesh $ skelly
