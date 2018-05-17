{-|
Module      : Genotype
Description : Monad for building genes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

Monad for building Genotypes and evaluating them in Parallel
Enable -XApplicativeDo for automatic parallelization
-}

{-# LANGUAGE DefaultSignatures #-}

module AnimalClub.Genetics.Genotype (
    -- * Monads
    -- $monaddoclink
    GenotypeT(..),
    Genotype,
    -- ** Monad evalution functions
    evalGeneBuilderT,
    evalGeneBuilder,
    -- * Gene building monad operations
    usingGene,
    gbDNALength,
    gbSum,
    gbNormalizedSum,
    gbSumRange,
    gbNormalizedThresh,
    gbTypical,
    gbRandomRanges
) where

import AnimalClub.Genetics.DNA
import AnimalClub.Genetics.Gene

import qualified Data.Vector.Unboxed as V

import Lens.Micro.Platform (over, _1, _2)
import Control.Parallel.Strategies
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.Parallel (MonadParallel(..))
import Control.Monad.Writer

import Control.Exception.Base (assert)

--import Debug.Trace

-- | StateT GenotypeState (WriterT w (RandT g m))
newtype GenotypeT g w m a = GenotypeT { unGenotypeT :: g -> DNA -> m (a, g, w) }
type Genotype g w = GenotypeT g w Identity

-- |
instance (Functor m) => Functor (GenotypeT g w m) where
    fmap f n = GenotypeT $ \g dna -> fmap (over _1 f) (unGenotypeT n g dna)

-- |
-- implemented using Monad m to ensure that (<*>) = ap (TEST)
instance (Monoid w, Monad m) => Applicative (GenotypeT g w m) where
    pure a = GenotypeT (\g _ -> pure (a, g, mempty))
    liftA2 f ma mb = do
        a <- ma
        b <- mb
        return $ f a b

-- |
instance (Monoid w, Monad m) => Monad (GenotypeT g w m) where
    return = pure
    ma >>= f = GenotypeT func where
        func g dna = do
            (a, g', w1) <- unGenotypeT ma g dna
            (b, g'', w2) <- unGenotypeT (f a) g' dna
            return (b, g'', mappend w1 w2)

instance (Monoid w) => MonadTrans (GenotypeT g w) where
    lift m = GenotypeT (\g dna -> m >>= (\a -> return (a, g, mempty)))

instance (Monoid w, Monad m) => MonadWriter w (GenotypeT g w m) where
    tell w = GenotypeT $ \g _ -> return ((), g, w)
    listen ma = GenotypeT func where
        func g dna = do
            (a, g', w) <- unGenotypeT ma g dna
            return ((a, w), g', w)
    pass ma = GenotypeT func where
        func g dna = do
            ((a, f), g', w) <- unGenotypeT ma g dna
            return (a, g', f w)


-- | minimal dna length requirement for automatic parallelization
genoTypeParMin :: Int
genoTypeParMin = 10

-- |
-- RandomGen g constraint required to split the generator for deterministic parallel evaluation (whether it's actually used or not)
instance forall w g m. (Monoid w, RandomGen g, MonadParallel m) => MonadParallel (GenotypeT g w m) where
    bindM2 f' ma mb = GenotypeT func where
        bindM2Serial f ma mb = do { a <- ma; b <- mb; f a b }
        func g dna = if dnaLength dna <= genoTypeParMin
            then unGenotypeT (bindM2Serial f' ma mb) g dna
            else bindM2 f ra rb where
                -- make generators
                (g',(g'',g''')) = over _2 split $ split g
                -- parallel evaluate a and b to produce c
                ra = unGenotypeT ma g' dna
                rb = unGenotypeT mb g'' dna
                -- unwrap and rewrap the monadic output of the inner monad
                f (x1,_,w1) (x2,_,w2) =
                    unGenotypeT (f' x1 x2) g''' dna
                    >>= \(c, g'''', w3) -> return (c, g'''', mconcat [w1,w2,w3])

-- |
-- constraints needed to satisfy Monad instance
instance (Monoid w, RandomGen g, Monad m) => MonadRandom (GenotypeT g w m) where
    getRandom = GenotypeT func where
        func g dna = return (a,g',mempty) where
            (a,g') = random g
    getRandomR r = GenotypeT func where
        func g dna = return (a,g',mempty) where
            (a,g') = randomR r g

-- | evalute the builder and obtain its output
evalGeneBuilderT :: (Monad m) => GenotypeT g w m a -> DNA -> g -> m w
evalGeneBuilderT m s g = do
    (_,_,w) <- unGenotypeT m g s
    return w

-- | evalute the builder and obtain its output
evalGeneBuilder :: Genotype g w a -> DNA -> g -> w
evalGeneBuilder m s g = runIdentity $ evalGeneBuilderT m s g

-- | apply a computation on a Gene
-- will error if Gene is out of bounds of DNA being operated on
usingGene :: Gene -> GenotypeT g w m a -> GenotypeT g w m a
usingGene (Gene i n) gt = GenotypeT $ \g dna -> unGenotypeT gt g (V.slice i n dna)

-- | return length of DNA being computed on
gbDNALength :: (Monoid w, Monad m) => GenotypeT g w m Int
gbDNALength = GenotypeT $ \g dna -> return (dnaLength dna, g, mempty)

-- | Computation that adds all genes of current genotype
gbSum :: (Monoid w, Monad m) => GenotypeT g w m Int
gbSum = GenotypeT $ \g dna -> return (dnaSum dna, g, mempty)

-- | gbSum normalized to [0,1]
gbNormalizedSum :: (Monoid w, Monad m) => GenotypeT g w m Float
gbNormalizedSum = do
    s <- gbSum
    l <- gbDNALength
    return $ if l == 0 then 0 else 0.125 * fromIntegral s / fromIntegral l

-- parallel version which I'm pretty sure runs slower
--gbNormalizedSum :: (RandomGen g, Monoid w, MonadParallel m) => GenotypeT g w m Float
--gbNormalizedSum = liftA2 (\s l -> 0.125 * fromIntegral s / fromIntegral l) gbSum gbDNALength

-- | Computation returns True if gbNormalizedSum > thresh, False otherwise
gbSumRange :: (Monoid w, Monad m) => (Float,Float) -> GenotypeT g w m Float
gbSumRange (min',max') = do
    s <- gbNormalizedSum
    return $ min' + s * (max'-min')

-- | Computation returns True if gbNormalizedSum > thresh, False otherwise
gbNormalizedThresh :: (Monoid w, Monad m) => Float -> GenotypeT g w m Bool
gbNormalizedThresh thresh = do
    s <- gbNormalizedSum
    return $ s > thresh



-- | Computation that sums a gene in two parts, treating the first part as a multiplier of the second part
-- first 1/4 is multiplicative, last 3/4 is additive.
gbTypical :: (Monoid w, Monad m) => (Float, Float) -> GenotypeT g w m Float
gbTypical (min_, max_) = do
    l <- gbDNALength
    let
        ml = l `quot` 4
    x <- usingGene (Gene 0 ml) gbNormalizedSum
    y <- usingGene (Gene ml (l-ml)) gbNormalizedSum
    return $ min_ + x * y * (max_ - min_)


-- | Computation that randomly creates several genes fitting the input range
gbRandomRanges :: (RandomGen g, Monoid w, Monad m) => [(Float, Float)] -> GenotypeT g w m [Float]
gbRandomRanges ranges = do
    gl <- gbDNALength
    let
        rl = length ranges
        l = gl `quot` rl
    return $ if l == 0 then error "genes too short" else ()
    forM [0..(rl-1)] $ \i -> do
        let
            (min_, max_) = ranges !! i
            short = gbNormalizedSum >>= \x -> return $ min_ + (max_-min_) * x
            long = gbTypical (min_, max_)
        usingGene (Gene (i*l) l) $ do
            rn <- getRandom
            if l < 20 || rn then short else long

-- | returns an 8 length array that counts occurrence of each bit
gbByteSample :: (Monoid w, Monad m) => GenotypeT g w m [Int]
gbByteSample = GenotypeT (\g dna -> return (V.toList (dnaBitCount dna), g, mempty))
