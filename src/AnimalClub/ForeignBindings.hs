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
{-# LANGUAGE UnboxedTuples            #-}

module AnimalClub.ForeignBindings (
) where

import           AnimalClub.Animals
import           AnimalClub.Animals.Examples
import           AnimalClub.Genetics
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Mesh

import           Data.Convertible
import qualified Data.Vector.Storable        as V
import           Foreign
import           Foreign.C.Types
--import           Foreign.Storable.Tuple
import           System.Random

import           Debug.Trace

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
-- TODO need DNA length parameter
type GoatSpecimen = (DNA, Genome StdGen [AnimalExp Float])

random_goat_hs :: CInt -> IO (StablePtr GoatSpecimen)
random_goat_hs len = do
  gen <- getStdGen
  let
    len' = convert len
    genome = makeGenomeFromPropertiesSimple len' [] goatPropertyList
    dna = makeRandDNA gen len'
  r <- newStablePtr (dna, genome)
  trace (show (castStablePtrToPtr r)) $ return r

free_goat_hs :: StablePtr GoatSpecimen -> IO ()
free_goat_hs = freeStablePtr


-- needed because Vector.Storable only gives us a ForeignPtr which will get GC'd if we lose all refs to ForeignPtr
type StableForeignPtr a = StablePtr (ForeignPtr a)
type StableCMesh = (StableForeignPtr CFloat, CInt, StableForeignPtr CInt, CInt)

goat_mesh_hs :: StablePtr GoatSpecimen -> IO (StablePtr StableCMesh)
goat_mesh_hs goatPtr = do
  (dna, goatGenome) <- deRefStablePtr goatPtr
  let
    goatProps = generateAnimalProperties (makeBoneIdList goatAnimalNode) $ evalGenome goatGenome dna
    skelly = animalNodeToSkellyNodeWithProps goatProps goatAnimalNode
    CMesh verts faces = toCMesh . generateLocalMesh $ skelly
    (vptr, vsz) = V.unsafeToForeignPtr0 verts
    (fptr, fsz) = V.unsafeToForeignPtr0 faces
  vsptr <- newStablePtr . castForeignPtr $ vptr
  fsptr <- newStablePtr . castForeignPtr $ fptr
  r <- newStablePtr (vsptr, convert vsz * 3, fsptr, convert fsz * 3)
  return r

free_goat_mesh_hs :: StablePtr StableCMesh -> IO ()
free_goat_mesh_hs ptr = do
  (pt1,_,pt2,_) <- deRefStablePtr ptr
  freeStablePtr pt1
  freeStablePtr pt2
  freeStablePtr ptr

foreign export ccall random_goat_hs :: CInt -> IO (StablePtr GoatSpecimen)
foreign export ccall free_goat_hs :: StablePtr GoatSpecimen -> IO ()
foreign export ccall goat_mesh_hs :: StablePtr GoatSpecimen -> IO (StablePtr StableCMesh)
foreign export ccall free_goat_mesh_hs :: StablePtr StableCMesh -> IO ()


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
