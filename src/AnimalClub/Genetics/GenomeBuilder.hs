{-|
Module      : GenomeBuilder
Description : Monad for building genes
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

Monad for building genes using Genotype.
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


module AnimalClub.Genetics.GenomeBuilder (
    -- * Monads
    -- $monaddoclink
    GenomeBuilderT,
    GenomeBuilder,
    -- ** Monad evalution functions
    evalGeneBuilderT,
    evalGeneBuilder,
    -- * Writer monad operations using NamedFloats
    NamedFloats,
    tellGene,
    tellGenes,
    prefixGenes,
    -- * Gene building monad operations
    -- | push and pop operate on the Genotype stack, while all other operations operate on the Genotype defined by the stack
    gbPush,
    gbPop,
    gbSum,
    gbNormalizedSum,
    gbNormalizedThresh,
    gbTypical,
    gbRandomRanges
) where

import AnimalClub.Genetics.Gene
import AnimalClub.Genetics.Genotype

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

-- $monaddoclink
-- The 'FastGenebuilder' monad contains a state object holding the underlying DNA and a Genotype stack representing the current scope of operations.
-- The monad also contains a Writer monad used to outputted computed values from the state object allowing the user to organize the output in any way they please.
-- All operations in this module treat the state DNA to be @read-only@ though there is no reason why it can't be modified

-- TODO write ParStateT Monad that takes advantage of applicative do to parallelize state transformations
-- i.e. tell/push/pop are the only things that change state, all operations in between
-- can be parallelized. c.f. haxl

type GeneBuilderState = (DNA, [Genotype])


-- | Writer monoid for either value or names
-- rename to StandardNamedFloats
type NamedFloats = [(Text, [Float])]


-- allow output type to be parameterized please
-- | Monad transformer for building genes
type GenomeBuilderT g w m = StateT GeneBuilderState (WriterT w (RandT g m))
-- | Monad for building genes
type GenomeBuilder g w = GenomeBuilderT g w Identity

-- | Write a single gene values in the builder
tellGene :: (Monad m) => Text -> Float -> GenomeBuilderT g NamedFloats m ()
tellGene s v = tellGenes s [v]

-- | Write several gene values in the builder
tellGenes :: (Monad m) => Text -> [Float] -> GenomeBuilderT g NamedFloats m ()
tellGenes s v = tell $ [(s, v)]

-- | prefix all output
-- good for duplicating output for L/R or something like that
prefixGenes :: (Monad m) => Text -> GenomeBuilderT g [(Text,b)] m a -> GenomeBuilderT g [(Text,b)] m a
prefixGenes s m = do
    a <- m
    pass . return $ (a, map (\(x,v) -> (append x s, v)))

-- | evalute the builder and obtain its output
evalGeneBuilderT :: (RandomGen g, Monoid w, Monad m) => GenomeBuilderT g w m a -> GeneBuilderState -> g -> m w
evalGeneBuilderT m s g = (flip evalRandT) g . execWriterT $ evalStateT m s

-- | evalute the builder and obtain its output
evalGeneBuilder :: (RandomGen g, Monoid w) => GenomeBuilder g w a -> GeneBuilderState -> g -> w
evalGeneBuilder m s g = runIdentity $ evalGeneBuilderT m s g

-- | internal helper function for folding Genotype hierarchies in the builder
absoluteGenotype :: GeneBuilderState -> Genotype
absoluteGenotype (dna, gtl) = foldr combineGenotype (Genotype 0 (4 * V.length dna)) gtl



-- | Push a genotype onto the hierarchy
gbPush :: (Monoid w, Monad m) => Genotype -> GenomeBuilderT g w m ()
gbPush gt = do
    (dna, gtl) <- get
    case gtl of
        (x:_) -> if geneLength gt >= geneLength x
            then error "Prepending Genotype with length greater than length of last Genotype on stack"
            else return ()
        _ -> return ()
    put (dna, gt:gtl)
    return ()

-- | Pop a genotype from the hierarchy
gbPop :: (Monoid w, Monad m) => GenomeBuilderT g w m ()
gbPop = do
    (dna, gtl) <- get
    case gtl of
        (_:xs) -> put (dna, xs)
        _ -> error "Popping an empty genotype stack."
    return ()


-- | Computation that adds all genes of current genotype
gbSum :: (Monoid w, Monad m) => GenomeBuilderT g w m Int
gbSum = state gbSum' where
    gbSum' s = (tryGeneSum (fst s) (absoluteGenotype s), s)


-- | Same as gbSum but normalized to [0,1]
gbNormalizedSum :: (Monoid w, Monad m) => GenomeBuilderT g w m Float
gbNormalizedSum = do
    state gbSum' where
    gbSum' (dna, gtl) = (answer, (dna, gtl)) where
        foldedGenotype = (absoluteGenotype (dna, gtl))
        sum_ = tryGeneSum dna foldedGenotype
        length_ = geneLength foldedGenotype
        answer = if length_ == 0 then 0 else 0.5 * fromIntegral sum_ / fromIntegral length_

-- | Computation returns True if gbNormalizedSum > thresh, False otherwise
gbNormalizedThresh :: (Monoid w, Monad m) => Float -> GenomeBuilderT g w m Bool
gbNormalizedThresh thresh = do
    s <- gbNormalizedSum
    return $ s > thresh

-- | Computation that sums a gene in two parts, treating the first part as a multiplier of the second part
-- first 1/4 is multiplicative, last 3/4 is additive.
gbTypical :: (Monoid w, Monad m) => (Float, Float) -> GenomeBuilderT g w m Float
gbTypical (min_, max_) = do
    (dna, gtl) <- get
    let
        l = geneCount (absoluteGenotype (dna, gtl))
        ml = l `quot` 4
    gbPush (Genotype 0 ml)
    mult <- gbNormalizedSum
    gbPop
    gbPush (Genotype ml (l-ml))
    add <- gbNormalizedSum
    gbPop
    return $ min_ + mult * add * (max_ - min_)


-- | Computation that randomly creates several genes fitting the input range
gbRandomRanges :: (RandomGen g, Monoid w, Monad m) => [(Float, Float)] -> GenomeBuilderT g w m [Float]
gbRandomRanges ranges = do
    (dna, gtl) <- get
    let
        rl = length ranges
        gl = geneCount (absoluteGenotype (dna, gtl))
        l = gl `quot` rl
    return $ assert (l > 0) ()
    forM [0..(rl-1)] $ \i -> do
        let
            (min_, max_) = ranges !! i
            short = gbNormalizedSum >>= \x -> return $ min_ + (max_-min_) * x
            long = gbTypical (min_, max_)
        gbPush (Genotype (i*l) l)
        rn <- getRandom
        output <- if (l < 20 || rn) then short else long
        gbPop
        return output







{-
-- TODO random stuff
GbRandomParams = GbRandomParams {
    maxDepth :: Int,
    maxNumChildren :: Int
    --random type weight parameter
}

--g is of class RandomGen
--type RandomGenotypeSubBuilder = State (g, )
--gbRandom :: (RandomGen g, Monoid w, Monad m) => Genotype -> GbRandomParams -> GenomeBuilderT m Float
-}
