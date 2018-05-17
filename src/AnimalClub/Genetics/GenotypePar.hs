{-|
Module      : GenotypePar
Description : Automatic parallelization using ApplicativeDo
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

Import this module and enable -XApplicativeDo for automatic parallelization
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module AnimalClub.Genetics.GenotypePar (
) where

import AnimalClub.Genetics.Genotype

import Control.Monad.Identity
import Control.Monad.Random
import Control.Applicative
import Control.Monad.Parallel


-- |
instance {-# OVERLAPPING #-} (Monoid w, RandomGen g) => Applicative (GenotypeT g w Identity) where
    liftA2 f = bindM2 (\a b -> return (f a b))
    pure a = GenotypeT (\g _ -> return (a, g, mempty))

-- |
instance {-# OVERLAPPING #-} (Monoid w, RandomGen g) => Applicative (GenotypeT g w IO) where
    liftA2 f = bindM2 (\a b -> return (f a b))
    pure a = GenotypeT (\g _ -> return (a, g, mempty))


-- |
-- implemented using bindM2 for automatic parallelization using ApplicativeDo
-- this forces the RandomGen g and MonadParallel m instance to be used everywhere
-- that should really just need Monad m so we don't do this. Actually I think this is
-- possible in ghc < 7.10 since Applicative is not a superclass of Monad
--instance (Monoid w, RandomGen g, MonadParallel m) => Applicative (GenotypeT g w m) where
--	liftA2 f = bindM2 (\a b -> return (f a b))
--	pure a = GenotypeT (\g _ -> return (a, g, mempty))
