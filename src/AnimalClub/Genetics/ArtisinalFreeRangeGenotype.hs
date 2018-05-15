{-|
Module      : Genotype
Description : Monad for building genes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

Monad for building genes using Gene.
-}

--{-# LANGUAGE KindSignatures            #-} -- needed to explictly declare (n::Nat)
--{-# LANGUAGE ExistentialQuantification #-} -- forall
--{-# LANGUAGE ExplicitNamespaces        #-} -- so I can import type (<=)
--{-# LANGUAGE TypeOperators             #-} -- so I can do (m <= n) on types
--{-# LANGUAGE DataKinds                 #-} -- so I can do data ___ (n::Nat)
--{-# LANGUAGE GADTs                     #-} -- data type constraints
{-# LANGUAGE TypeSynonymInstances        #-} -- allows: instance ___ SomeTypeSynonym
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses     #-} -- so I can do a multiparameter type class


module AnimalClub.Genetics.ArtisinalFreeRangeGenotype (
    -- * Monads
    -- $monaddoclink
    GenotypeT,
    Genotype,
    -- ** Monad evalution functions
    evalGeneBuilderT,
    evalGeneBuilder,
    -- * Gene building monad operations
    -- | push and pop operate on the Gene stack, while all other operations operate on the Gene defined by the stack
) where

import AnimalClub.Genetics.DNA
import AnimalClub.Genetics.Gene

import Data.Text (Text, append)
import qualified Data.Vector.Unboxed as V
import Control.Monad.Writer.Lazy (WriterT, tell, pass, execWriterT)
import Control.Monad.State.Lazy (StateT, evalStateT, put, get, state)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Random (RandT, evalRandT, getRandom)
import Control.Monad (forM)
import System.Random (RandomGen)
import Control.Exception.Base (assert)

--import Debug.Trace

-- | StateT GenotypeState (WriterT w (RandT g m))
newtype GenotypeT g w m a = GenotypeT { unGenotypeT :: g -> DNA -> m (a, g, w) }
type Genotype g w = GenotypeT g w Identity

-- | evalute the builder and obtain its output
evalGeneBuilderT :: (RandomGen g, Monoid w, Monad m) => GenotypeT g w m a -> DNA -> g -> m w
evalGeneBuilderT m s g = do
    (_,_,w) <- unGenotypeT m g s
    return w

-- | evalute the builder and obtain its output
evalGeneBuilder :: (RandomGen g, Monoid w) => Genotype g w a -> DNA -> g -> w
evalGeneBuilder m s g = runIdentity $ evalGeneBuilderT m s g

addGene :: Int -> Int -> GenotypeT g w m a -> GenotypeT g w m a
addGene i n gt = GenotypeT $ \g dna -> unGenotypeT gt g (V.slice i n dna)
