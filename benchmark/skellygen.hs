{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import           Relude

import           AnimalClub.Animals.Examples

import           AnimalClub.Animals
import           AnimalClub.Genetics
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Mesh

import           Control.DeepSeq
import           System.Random

import           Criterion.Main

main :: IO ()
main = do
  let
    -- this is just to generate a dummy set of props
    goatProps = generateAnimalProperties (makeBoneIdList goatAnimalNode) $ evalGenome goatGenome original where
      goatDNALength = 1000
      goatGenome = makeGenomeFromPropertiesSimple goatDNALength [] goatPropertyList
      original = makeRandDNA (mkStdGen 0) goatDNALength
    skelly = animalNodeToSkellyNode goatAnimalNode
    potatoMesh = generatePotatoMesh skelly
  -- force input values before testing
  goatAnimalNode `deepseq` skelly `deepseq` potatoMesh `deepseq` return ()
  defaultMain [
      bgroup "animalNodeToSkellyNodeWithProps"
        [bench "goat" $ nf (animalNodeToSkellyNodeWithProps goatProps) goatAnimalNode]
      , bgroup "animalNodeToSkellyNode"
        [bench "goat" $ nf animalNodeToSkellyNode goatAnimalNode]
      , bgroup "generatePotatoMesh"
        [bench "goat" $ nf generatePotatoMesh skelly]
      , bgroup "potatoMeshToObj"
        [bench "goat" $ nf potatoMeshToObj potatoMesh]
    ]
