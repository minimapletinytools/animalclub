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
import           AnimalClub.Skellygen.Linear  hiding (trace)
import           AnimalClub.Skellygen.Mesh

import           Data.Convertible
import           Data.Int                     (Int32)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
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

goatSize :: Int
goatSize = 10000

goatGenome :: Genome StdGen [AnimalExp Float]
goatGenome = makeGenomeFromPropertiesSimple goatSize [] goatPropertyList

random_goat_hs :: IO (StablePtr DNA)
random_goat_hs = do
 gen <- getStdGen
 newStablePtr (makeRandDNA gen goatSize)

free_goat_hs :: StablePtr DNA -> IO ()
free_goat_hs = freeStablePtr

breed_goat_hs :: StablePtr DNA -> StablePtr DNA -> IO (StablePtr DNA)
breed_goat_hs ptr1 ptr2 = do
 dna1 <- deRefStablePtr ptr1
 dna2 <- deRefStablePtr ptr2
 gen <- getStdGen
 newStablePtr $ breed gen dna1 dna2

type CCMesh = (Ptr CFloat, CInt, Ptr CInt, CInt)

-- | caller is responsible for freeing memory by calling free_goat_mesh_hs
goat_mesh_hs :: StablePtr DNA -> IO (Ptr CCMesh)
goat_mesh_hs goatPtr = do
 dna <- deRefStablePtr goatPtr
 let
  goatProps = generateAnimalProperties (makeBoneIdList goatAnimalNode) $ evalGenome goatGenome dna
  skelly = animalNodeToSkellyNodeWithProps goatProps goatAnimalNode
  CMesh verts faces = toCMesh . generateLocalMesh $ skelly
  vl = V.length verts
  fl = V.length faces
 vptr <- mallocArray vl :: IO (Ptr (V3 Float))
 fptr <- mallocArray fl :: IO (Ptr (Int32,Int32,Int32))
 vfptr <- newForeignPtr_ vptr
 ffptr <- newForeignPtr_ fptr
 V.copy (MV.unsafeFromForeignPtr0 vfptr vl) verts
 V.copy (MV.unsafeFromForeignPtr0 ffptr fl) faces
 r <- new (castPtr vptr, convert vl * 3, castPtr fptr, convert fl * 3)
 return r

free_goat_mesh_hs :: Ptr CCMesh -> IO ()
free_goat_mesh_hs ptr = do
 (pt1,_,pt2,_) <- peek ptr
 free pt1
 free pt2
 free ptr

dump_goat_hs :: StablePtr DNA -> IO ()
dump_goat_hs goatPtr = do
 dna <- deRefStablePtr goatPtr
 let
  goatProps = generateAnimalProperties (makeBoneIdList goatAnimalNode) $ evalGenome goatGenome dna
  skelly = animalNodeToSkellyNodeWithProps goatProps goatAnimalNode
  mesh = generateLocalMesh $ skelly
 putStrLn (meshToObj mesh)

foreign export ccall random_goat_hs :: IO (StablePtr DNA)
foreign export ccall free_goat_hs :: StablePtr DNA -> IO ()
foreign export ccall breed_goat_hs :: StablePtr DNA -> StablePtr DNA -> IO (StablePtr DNA)
foreign export ccall goat_mesh_hs :: StablePtr DNA -> IO (Ptr CCMesh)
foreign export ccall free_goat_mesh_hs :: Ptr CCMesh -> IO ()
foreign export ccall dump_goat_hs :: StablePtr DNA -> IO ()


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
