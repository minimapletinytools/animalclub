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
  breed_hs
) where

import           AnimalClub.Genetics.DNA
import           Data.Convertible
import qualified Data.Vector.Storable    as V
import           Foreign
import           Foreign.C.Types
import           Foreign.ForeignPtr
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
  gen <- getStdGen
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

-- TODO figure out how to create genome
{- dnaProps_hs ::
  Ptr CChar -- ^ DNA bytes
  -> CSize -- ^ DNA size
  -> Genome
  ->

  -}
