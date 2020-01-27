{-|
Module      : ForeignBindings
Description : Foreign bindings for animal club
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

This module exports a bunch of methods in AnimalClub via FFI

-}

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module AnimalClub.ForeignBindings (
) where

import           AnimalClub.Animals
import           AnimalClub.Animals.Examples
import           AnimalClub.Genetics

import           Data.Convertible
import qualified Data.Vector.Storable        as V
import           Foreign
import           Foreign.C.Types
import           System.Random


breed_hs ::
  CInt -- ^ random seed
  -> Ptr CChar -- ^ DNA bytes 1
  -> Ptr CChar -- ^ DNA bytes 2
  -> Ptr CChar -- ^ DNA output
  -> CSize -- ^ DNA size
  -> IO () -- ^ 😱
breed_hs seed dna1' dna2' outdna' size = do
  dna1 <- newForeignPtr_ . castPtr $ dna1'
  dna2 <- newForeignPtr_ . castPtr $ dna2'
  outdna <-  newForeignPtr_ . castPtr $ outdna'
  let
    gen = mkStdGen (convert seed)
    dna1v = V.unsafeFromForeignPtr0 dna1 (convert size) :: DNA
    dna2v = V.unsafeFromForeignPtr0 dna2 (convert size) :: DNA
    outdnav = V.unsafeFromForeignPtr0 outdna (convert size) :: DNA
    -- TODO implement an MVector version of breed that stores results in place
    newdna = breed gen dna1v dna2v
  outdnamv <- V.unsafeThaw outdnav
  V.copy outdnamv newdna

foreign export ccall breed_hs :: CInt -> Ptr CChar -> Ptr CChar -> Ptr CChar -> CSize -> IO ()




-- | stuff below is specific to goats
-- consider moving this into the Animals.Examples folder
type Goat = (DNA, Genome StdGen [AnimalExp Float])

random_goat :: CInt -> IO (StablePtr Goat)
random_goat len = do
  gen <- getStdGen
  let
    len' = convert len
    genome = makeGenomeFromPropertiesSimple len' [] goatPropertyList
    dna = makeRandDNA gen len'
  newStablePtr (dna, genome)

free_goat :: StablePtr Goat -> IO ()
free_goat = freeStablePtr


foreign export ccall random_goat :: CInt -> IO (StablePtr Goat)
foreign export ccall free_goat :: StablePtr Goat -> IO ()

--goatObj :: StablePtr Goat -> IO
--goatObj goatPtr = do
  -- goat <- deRefStablePtr goatPtr
  --goatProps = generateAnimalProperties (makeBoneIdList goat) $ evalGenome goatGenome original
  --skelly = animalNodeToSkellyNodeWithProps goatProps goat
  --writeFile "wigglygoat.obj" . meshToObj . generateMesh $ skelly


-- TODO figure out how to create genome
{- dnaProps_hs ::
  Ptr CChar -- ^ DNA bytes
  -> CSize -- ^ DNA size
  -> Genome
  ->

  -}
