-- WORK IN PROGRESS

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

import qualified Data.Vector.Unboxed as V

import Lens.Micro.Platform (over, _1, _2)
import Control.Parallel.Strategies
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Random (MonadRandom(..), RandomGen(..))
import Control.Monad.Parallel (MonadParallel(..))

import Control.Exception.Base (assert)

--import Debug.Trace

-- | StateT GenotypeState (WriterT w (RandT g m))
newtype GenotypeT g w m a = GenotypeT { unGenotypeT :: g -> DNA -> m (a, g, w) }
type Genotype g w = GenotypeT g w Identity

instance (Functor m) => Functor (GenotypeT g w m) where
	fmap f n = GenotypeT $ \g dna -> fmap (over _1 f) (unGenotypeT n g dna)

-- |
-- implemented using bindM2 for automatic parallelization using ApplicativeDo
instance (Monoid w, RandomGen g, MonadParallel m) => Applicative (GenotypeT g w m) where
	liftA2 f = bindM2 (\a b -> return (f a b))

-- |
-- requires MonadParallel m and RandomGen g constraints to support automatic parallelization using ApplicativeDo
-- necessary as of GHC 7.10 now that Applicative is a superclass of Monad which is
-- kind of an unfortunate consequence of an otherwise good change
instance (Monoid w, RandomGen g, MonadParallel m) => Monad (GenotypeT g w m) where
    return a = GenotypeT (\g _ -> return (a, g, mempty))
    ma >>= f = GenotypeT func where
        func g dna = do
            (a, g', w1) <- unGenotypeT ma g dna
            (b, g'', w2) <- unGenotypeT (f a) g' dna
            return (b, g'', mappend w1 w2)

-- |
-- RandomGen g constraint required to split the generator for deterministic parallel evaluation (whether it's actually used or not)
instance forall w g m. (Monoid w, RandomGen g, MonadParallel m) => MonadParallel (GenotypeT g w m) where
    bindM2 :: forall a b c. (a -> b -> GenotypeT g w m c) -> GenotypeT g w m a -> GenotypeT g w m b -> GenotypeT g w m c
    bindM2 f' a' b' = GenotypeT func where
        func :: g -> DNA -> m (c, g, w)
        func g dna = bindM2 f ra rb where
            -- make generators
            (g',(g'',g''')) = over _2 split $ split g
            -- parallel evaluate a and b to produce c
            ra = unGenotypeT a' g' dna
            rb = unGenotypeT b' g'' dna
            -- unwrap and rewrap the monadic output of the inner monad
            f :: (a, g, w) -> (b, g, w) -> m (c, g, w)
            f (x1,_,w1) (x2,_,w2) =
                unGenotypeT (f' x1 x2) g''' dna
                >>= \(c, g'''', w3) -> return (c, g'''', mconcat [w1,w2,w3])

-- |
-- constraints needed to satisfy Monad instance
instance (Monoid w, RandomGen g, MonadParallel m) => MonadRandom (GenotypeT g w m) where
    getRandom = undefined
    getRandomR r = undefined

-- | evalute the builder and obtain its output
evalGeneBuilderT :: (RandomGen g, Monoid w, Monad m) => GenotypeT g w m a -> DNA -> g -> m w
evalGeneBuilderT m s g = do
    (_,_,w) <- unGenotypeT m g s
    return w

-- | evalute the builder and obtain its output
evalGeneBuilder :: (RandomGen g, Monoid w) => Genotype g w a -> DNA -> g -> w
evalGeneBuilder m s g = runIdentity $ evalGeneBuilderT m s g

-- | apply a computation on a Gene
usingGene :: Gene -> GenotypeT g w m a -> GenotypeT g w m a
usingGene (Gene i n) gt = GenotypeT $ \g dna -> unGenotypeT gt g (V.slice i n dna)

-- | Computation that adds all genes of current genotype
gbSum :: (Monoid w, Monad m) => GenotypeT g w m Int
gbSum = GenotypeT $ \g dna -> return (dnaSum dna, g, mempty)

gbDNALength :: (Monoid w, Monad m) => GenotypeT g w m Int
gbDNALength = GenotypeT $ \g dna -> return (dnaLength dna, g, mempty)

gbNormalizedSum :: (RandomGen g, Monoid w, MonadParallel m) => GenotypeT g w m Float
gbNormalizedSum = liftA2 (\s l -> 0.125 * fromIntegral s / fromIntegral l) gbSum gbDNALength
