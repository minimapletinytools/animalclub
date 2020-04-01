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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AnimalClub.ForeignBindings (
) where

import           AnimalClub.Animals
import           AnimalClub.Animals.Examples
import           AnimalClub.Genetics
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Linear  hiding (trace)
import           AnimalClub.Skellygen.Mesh

import           Relude

import           Data.Convertible
import           Data.Int                     (Int32)
import qualified Data.Text                    as T
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Foreign
import           Foreign.C.Types
import           Foreign.Storable             (Storable (..))
import qualified Foreign.Storable.Record      as Store
import           System.Random

import           Debug.Trace


instance (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f, Storable g, Storable h) => Storable (a,b,c,d,e,f,g,h) where
   sizeOf    = Store.sizeOf storeHextuple
   alignment = Store.alignment storeHextuple
   peek      = Store.peek storeHextuple
   poke      = Store.poke storeHextuple

{-# INLINE storeHextuple #-}
storeHextuple ::
  (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f, Storable g, Storable h) =>
  Store.Dictionary (a,b,c,d,e,f,g,h)
storeHextuple =
   Store.run $
   pure (,,,,,,,)
      <*> (Store.element $ \(x,_,_,_,_,_,_,_) -> x)
      <*> (Store.element $ \(_,x,_,_,_,_,_,_) -> x)
      <*> (Store.element $ \(_,_,x,_,_,_,_,_) -> x)
      <*> (Store.element $ \(_,_,_,x,_,_,_,_) -> x)
      <*> (Store.element $ \(_,_,_,_,x,_,_,_) -> x)
      <*> (Store.element $ \(_,_,_,_,_,x,_,_) -> x)
      <*> (Store.element $ \(_,_,_,_,_,_,x,_) -> x)
      <*> (Store.element $ \(_,_,_,_,_,_,_,x) -> x)


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

goatGenome :: Genome StdGen [AnimalExp Float [Float]]
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

type CCMesh = (Ptr CFloat, CInt, Ptr CFloat, CInt, Ptr CFloat, CInt, Ptr CInt, CInt)

-- | caller is responsible for freeing memory by calling free_goat_mesh_hs
goat_mesh_hs :: StablePtr DNA -> IO (Ptr CCMesh)
goat_mesh_hs goatPtr = do
  dna <- deRefStablePtr goatPtr
  let
    goatProps = generateAnimalProperties (makeBoneIdList goatAnimalNode) $ evalGenome goatGenome dna
    skelly = animalNodeToSkellyNodeWithProps goatProps goatAnimalNode
    PotatoMesh v n tc f = generatePotatoMesh $ skelly
    vl = V.length v
    nl = V.length n
    tcl = V.length tc
    fl = V.length f
  vptr <- mallocArray vl :: IO (Ptr (V3 Float))
  nptr <- mallocArray nl :: IO (Ptr (V3 Float))
  tcptr <- mallocArray tcl :: IO (Ptr (V2 Float))
  fptr <- mallocArray fl :: IO (Ptr (Int32,Int32,Int32))
  vfptr <- newForeignPtr_ vptr
  nfptr <- newForeignPtr_ nptr
  tcfptr <- newForeignPtr_ tcptr
  ffptr <- newForeignPtr_ fptr
  V.copy (MV.unsafeFromForeignPtr0 vfptr vl) v
  V.copy (MV.unsafeFromForeignPtr0 nfptr vl) n
  V.copy (MV.unsafeFromForeignPtr0 tcfptr vl) tc
  V.copy (MV.unsafeFromForeignPtr0 ffptr fl) f
  r <- new (
      castPtr vptr, convert vl * 3,
      castPtr nptr, convert nl * 3,
      castPtr tcptr, convert tcl * 2,
      castPtr fptr, convert fl * 3
    )
  return r

free_goat_mesh_hs :: Ptr CCMesh -> IO ()
free_goat_mesh_hs ptr = do
  (pt1,_,pt2,_,pt3,_,pt4,_) <- peek ptr
  free pt1
  free pt2
  free pt3
  free pt4
  free ptr

dump_goat_hs :: StablePtr DNA -> IO ()
dump_goat_hs goatPtr = do
  dna <- deRefStablePtr goatPtr
  let
    goatProps = generateAnimalProperties (makeBoneIdList goatAnimalNode) $ evalGenome goatGenome dna
    skelly = animalNodeToSkellyNodeWithProps goatProps goatAnimalNode
    mesh = generatePotatoMesh $ skelly
  putStrLn $ T.unpack (potatoMeshToObj mesh)

foreign export ccall random_goat_hs :: IO (StablePtr DNA)
foreign export ccall free_goat_hs :: StablePtr DNA -> IO ()
foreign export ccall breed_goat_hs :: StablePtr DNA -> StablePtr DNA -> IO (StablePtr DNA)
foreign export ccall goat_mesh_hs :: StablePtr DNA -> IO (Ptr CCMesh)
foreign export ccall free_goat_mesh_hs :: Ptr CCMesh -> IO ()
foreign export ccall dump_goat_hs :: StablePtr DNA -> IO ()


-- TODO figure out how to create genome
{- dnaProps_hs ::
 Ptr CChar -- ^ DNA bytes
 -> CSize -- ^ DNA size
 -> Genome
 ->

 -}
