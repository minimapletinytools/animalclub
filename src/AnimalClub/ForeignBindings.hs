{-|
Module      : ForeignBindings
Description : Foreign bindings for animal club
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

TODO
-}

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module AnimalClub.ForeignBindings (
  breed_hs
) where

import           Foreign
import           Foreign.C.Types

breed_hs ::
  CInt -- ^ random seed
  -> Ptr CChar -- ^ DNA bytes 1
  -> Ptr CChar -- ^ DNA bytes 2
  -> Ptr CChar -- ^ DNA output
  -> CSize -- ^ DNA size
  -> IO () -- ^ 😱
breed_hs seed dna1 dna2 outdna size = undefined

foreign export ccall breed_hs :: CInt -> Ptr CChar -> Ptr CChar -> Ptr CChar -> CSize -> IO ()

-- TODO figure out how to create genome
{- dnaProps_hs ::
  Ptr CChar -- ^ DNA bytes
  -> CSize -- ^ DNA size
  -> Genome
  ->

  -}
