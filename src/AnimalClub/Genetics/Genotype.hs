{-|
Module      : Genotype
Description : Monad for building genes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

Monad for building Genotypes and evaluating them in Parallel
New version using an artisinal free range monad
NOTE parallelization stuff is not actually running in parallel for some reason
-}

module AnimalClub.Genetics.Genotype (
    -- * Monads
    -- $monaddoclink
    GenotypeT(..),
    Genotype,
    -- ** Monad evalution functions
    evalGeneBuilderT,
    evalGeneBuilder,
    unGenotype,
    -- * Gene building monad operations
    usingGene,
    gbDNA,
    gbDNALength,
    gbSum,
    gbNormalizedSum,
    gbSumRange,
    gbNormalizedThresh,
    gbTypical,
    gbRandomRanges,
    gbByteSample1,
    --gbByteSample2,
    --gbBytePattern4,
    gbBytePattern
) where

import           AnimalClub.Genetics.DNA
import           AnimalClub.Genetics.Gene

import           Data.Bits
import qualified Data.Vector.Generic      as G
import           Data.Word

import           Control.Monad.Identity
import           Control.Monad.Parallel   (MonadParallel (..))
import           Control.Monad.Random
import           Control.Monad.Writer
import           Debug.Trace
import           Lens.Micro.Platform      (over, _1)

-- | this is just `StateT DNA (WriterT w (RandT g m))` unrolled
-- Genotype is a Writer monad taking an taking an RNG and DNA as inputs
newtype GenotypeT g w m a = GenotypeT { unGenotypeT :: g -> DNA -> m (a, g, w) }
type Genotype g w = GenotypeT g w Identity

-- | evalute the builder and obtain its output
unGenotype :: Genotype g w a -> DNA -> g -> (a, g, w)
unGenotype m s g = runIdentity $  unGenotypeT m g s

-- | evalute the builder and obtain its output
evalGeneBuilderT :: (Monad m) => GenotypeT g w m a -> DNA -> g -> m w
evalGeneBuilderT m s g = do
    (_,_,w) <- unGenotypeT m g s
    return w

-- | evalute the builder and obtain its output
evalGeneBuilder :: Genotype g w a -> DNA -> g -> w
evalGeneBuilder m s g = runIdentity $ evalGeneBuilderT m s g

-- |
instance (Functor m) => Functor (GenotypeT g w m) where
    fmap f n = GenotypeT $ \g dna -> fmap (over _1 f) (unGenotypeT n g dna)

-- |
-- implemented using Monad m to ensure that (<*>) = ap (TEST)
instance (Monoid w, Monad m) => Applicative (GenotypeT g w m) where
    pure a = GenotypeT (\g _ -> pure (a, g, mempty))
    (<*>) mf ma = do
        f <- mf
        a <- ma
        return $ f a
{- doesn't seem to be in older version of GHC
    liftA2 f ma mb = do
        a <- ma
        b <- mb
        return $ f a b
-}

-- |
instance (Monoid w, Monad m) => Monad (GenotypeT g w m) where
    return = pure
    ma >>= f = GenotypeT func where
        func g dna = do
            (a, g', w1) <- unGenotypeT ma g dna
            (b, g'', w2) <- unGenotypeT (f a) g' dna
            return (b, g'', mappend w1 w2)

instance (Monoid w) => MonadTrans (GenotypeT g w) where
    lift m = GenotypeT (\g _ -> m >>= (\a -> return (a, g, mempty)))

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
-- revert to sequential evaluation if the domain is small
-- note, this isn't great because a small domain does not mean a fast computation or vice versa
-- but we expect them to be reasonably correlated in most cases
genotypeParMin :: Int
genotypeParMin = 10

-- |
-- RandomGen g constraint required to split the generator for deterministic parallel evaluation (whether it's actually used or not)
-- NOTE parallel evaluation and serial evaluation produce different results due to par evaluation of RNG
-- I think we can fix this by changing instance definition of Monad Genotype but why bother
-- TODO/NOTE this is currently not actually creating any sparks or when it does they all GC D:
-- It might be because it evaluate the output tuple to only whnf?
instance forall w g m. (Monoid w, RandomGen g, MonadParallel m) => MonadParallel (GenotypeT g w m) where
    bindM2 f' ma mb = GenotypeT func where
        -- sequential evaluation for small domains, see comments in genotypeParMin
        bindM2Serial f ma' mb' = do { a <- ma'; b <- mb'; f a b }
        func g dna = if dnaLength dna <= genotypeParMin
            then unGenotypeT (bindM2Serial f' ma mb) g dna
            -- the inner monad should spark ra and rb :\
            --else ra `par` rb `pseq` bindM2 f ra rb where
            else bindM2 f ra rb where
                -- make generators
                --(g', g'') = trace "split g" $ split g
                (g', g'') = split g
                -- parallel evaluate a and b to produce c
                ra = unGenotypeT ma g' dna
                rb = unGenotypeT mb g'' dna
                -- unwrap and rewrap the monadic output of the inner monad
                -- to contain the results of ra and rb
                f (x1,_,w1) (x2,_,w2) =
                    unGenotypeT (f' x1 x2) g dna
                    >>= \(c, g_out, w3) -> return (c, g_out, mconcat [w1,w2,w3])
                    -- not sure why I wrote a version with seq here. force evaluating final output is not responsibility of this function
                    -- pretty sure should delete
                    -- >>= \(c, g_out, w3) -> c `seq` return (c, g_out, mconcat [w1,w2,w3])


-- |
-- constraints needed to satisfy Monad instance
instance (Monoid w, RandomGen g, Monad m) => MonadRandom (GenotypeT g w m) where
    getRandom = GenotypeT func where
        func g _ = return (a,g',mempty) where
            (a,g') = random g
    getRandomR r = GenotypeT func where
        func g _ = return (a,g',mempty) where
            (a,g') = randomR r g
    -- TODO
    getRandoms = undefined
    getRandomRs = undefined

-- | apply a computation on a Gene
-- this creates a new computation where DNA is the subsection of the original DNA as defined by the gene
-- will throw an error if Gene is out of bounds of DNA being operated on
usingGene :: Gene -> GenotypeT g w m a -> GenotypeT g w m a
usingGene gene gt = GenotypeT $ \g dna -> unGenotypeT gt g (extractDNA gene dna)

-- | return the DNA so you can do whatever on it
gbDNA :: (Monoid w, Monad m) => GenotypeT g w m DNA
gbDNA = GenotypeT $ \g dna -> return (dna, g, mempty)

-- | return length of DNA being computed on
gbDNALength :: (Monoid w, Monad m) => GenotypeT g w m Int
gbDNALength = GenotypeT $ \g dna -> return (dnaLength dna, g, mempty)

-- | Computation that adds all genes of current genotype
gbSum :: (Num a, Monoid w, Monad m) => GenotypeT g w m a
gbSum = GenotypeT $ \g dna -> return (dnaSum dna, g, mempty)

-- | gbSum normalized to [0,1]
gbNormalizedSum :: (Fractional a, Monoid w, Monad m) => GenotypeT g w m a
gbNormalizedSum = do
    s <- gbSum
    l <- gbDNALength
    return $ if l == 0 then 0 else 0.125 * fromIntegral s / fromIntegral l

-- parallel version which I'm pretty sure runs slower
--gbNormalizedSum :: (RandomGen g, Monoid w, MonadParallel m) => GenotypeT g w m Float
--gbNormalizedSum = liftA2 (\s l -> 0.125 * fromIntegral s / fromIntegral l) gbSum gbDNALength

-- | Computation returns True if gbNormalizedSum > thresh, False otherwise
gbSumRange :: (Fractional a, Monoid w, Monad m) => (a,a) -> GenotypeT g w m a
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
gbByteSample1 :: (Monoid w, Monad m) => GenotypeT g w m [Int]
gbByteSample1 = GenotypeT (\g dna -> return (G.toList (dnaBitCount dna), g, mempty))

{-
-- | returns a 4 length array that counts occurrence of [00,01,10,11]
-- on non-overlapping intervals of 2 bits
-- TODO finish
gbByteSample2 :: (Monoid w, Monad m) => GenotypeT g w m [Int]
gbByteSample2 = undefined

-- | counts occurrence of given patterns on non-overlapping intervals of 4 bits
-- argument is supplied as a pair of 2 4 bit patterns as a Word8
-- and output is number occurrences of these 2 patterns as a tuple
-- TODO finish
gbBytePattern4 :: (Monoid w, Monad m) => Word8 -> GenotypeT g w m (Int, Int)
gbBytePattern4 p = GenotypeT f where
    f g dna = return (G.foldl' ff (0,0) dna, g, mempty) where
        ff acc x = undefined
-}

-- | counts occurrence of given pattern on non-overlapping intervals of 8 bits
gbBytePattern :: (Monoid w, Monad m) => Word8 -> GenotypeT g w m Int
gbBytePattern p = GenotypeT f where
    f g dna = return (G.foldl' ff 0 dna, g, mempty) where
        ff acc x = if p `xor` x == 0 then acc + 1 else acc
