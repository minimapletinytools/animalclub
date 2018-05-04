{-|
Module      : FastGeneBuilder
Description : Monad for building genes 
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

Monad for building genes using FastGenotype.
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


module AnimalClub.Genetics.FastGeneBuilder (
    -- * Monads
    -- $monaddoclink
    FastGeneBuilderT,
    FastGeneBuilder,
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

import Data.Text (Text(..), append)
import Data.Either (Either(..))
import qualified Data.Vector.Unboxed as V 
import Control.Monad.Writer.Lazy (WriterT, tell, pass, execWriterT)
import Control.Monad.State.Lazy (StateT, evalStateT, put, get, state)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Random (RandT, evalRandT, getRandom)
import Control.Monad (forM)
import System.Random (RandomGen, Random)
import Control.Exception.Base (assert)

--import Debug.Trace

-- $monaddoclink 
-- The 'FastGenebuilder' monad contains a state object holding the underlying DNA and a Genotype stack representing the current scope of operations.
-- The monad also contains a Writer monad used to outputted computed values from the state object allowing the user to organize the output in any way they please.
-- All operations in this module treat the state DNA to be @read-only@ though there is no reason why it can't be modified

-- TODO write ParStateT Monad that takes advantage of applicative do to parallelize state transformations
-- i.e. tell/push/pop are the only things that change state, all operations in between
-- can be parallelized. c.f. haxl
 
type GeneBuilderState = (DNA, [FastGenotype])


-- | Writer monoid for either value or names
-- rename to StandardNamedFloats
type NamedFloats = [(Text, [Float])]


-- allow output type to be parameterized please
-- | Monad transformer for building genes
type FastGeneBuilderT g w m = StateT GeneBuilderState (WriterT w (RandT g m))
-- | Monad for building genes
type FastGeneBuilder g w = FastGeneBuilderT g w Identity

-- | Write a single gene values in the builder
tellGene :: (RandomGen g, Monad m) => Text -> Float -> FastGeneBuilderT g NamedFloats m ()
tellGene s v = tellGenes s [v]

-- | Write several gene values in the builder
tellGenes :: (RandomGen g, Monad m) => Text -> [Float] -> FastGeneBuilderT g NamedFloats m ()
tellGenes s v = tell $ [(s, v)]

-- | prefix all output 
-- good for duplicating output for L/R or something like that
prefixGenes :: (RandomGen g, Monad m) => Text -> FastGeneBuilderT g [(Text,b)] m a -> FastGeneBuilderT g [(Text,b)] m a
prefixGenes s m = do 
    a <- m
    pass . return $ (a, map (\(x,v) -> (append x s, v)))

-- | evalute the builder and obtain its output
evalGeneBuilderT :: (RandomGen g, Monoid w, Monad m) => FastGeneBuilderT g w m a -> GeneBuilderState -> g -> m w
evalGeneBuilderT m s g = (flip evalRandT) g . execWriterT $ evalStateT m s

-- | evalute the builder and obtain its output
evalGeneBuilder :: (RandomGen g, Monoid w) => FastGeneBuilder g w a -> GeneBuilderState -> g -> w
evalGeneBuilder m s g = runIdentity $ evalGeneBuilderT m s g

-- | internal helper function for folding Genotype hierarchies in the builder
absoluteGenotype :: GeneBuilderState -> FastGenotype
absoluteGenotype (dna, gtl) = foldr combineFastGenotype (FastGenotype 0 (4 * V.length dna)) gtl



-- | Push a genotype onto the hierarchy
gbPush :: (RandomGen g, Monoid w, Monad m) => FastGenotype -> FastGeneBuilderT g w m ()
gbPush gt = do
    (dna, gtl) <- get
    case gtl of 
        (x:xs) -> if geneLength gt >= geneLength x 
            then error "Prepending Genotype with length greater than length of last Genotype on stack" 
            else return ()
        _ -> return ()
    put (dna, gt:gtl)
    return ()

-- | Pop a genotype from the hierarchy
gbPop :: (RandomGen g, Monoid w, Monad m) => FastGeneBuilderT g w m ()
gbPop = do
    (dna, gtl) <- get
    case gtl of 
        (x:xs) -> put (dna, xs)
        _ -> error "Popping an empty genotype stack."
    return ()


-- | Computation that adds all genes of current genotype
gbSum :: (RandomGen g, Monoid w, Monad m) => FastGeneBuilderT g w m Int
gbSum = state gbSum' where
    gbSum' s = (tryGeneSum (fst s) (absoluteGenotype s), s)


-- | Same as gbSum but normalized to [0,1]
gbNormalizedSum :: (RandomGen g, Monoid w, Monad m) => FastGeneBuilderT g w m Float
gbNormalizedSum = do
    state gbSum' where
    gbSum' (dna, gtl) = (answer, (dna, gtl)) where
        foldedGenotype = (absoluteGenotype (dna, gtl))
        sum = tryGeneSum dna foldedGenotype
        length = geneLength foldedGenotype
        answer = if length == 0 then 0 else 0.5 * fromIntegral sum / fromIntegral length

-- | Computation returns True if gbNormalizedSum > thresh, False otherwise
gbNormalizedThresh :: (RandomGen g, Monoid w, Monad m) => Float -> FastGeneBuilderT g w m Bool
gbNormalizedThresh thresh = do
    s <- gbNormalizedSum
    return $ s > thresh

-- | Computation that sums a gene in two parts, treating the first part as a multiplier of the second part
-- first 1/4 is multiplicative, last 3/4 is additive. 
gbTypical :: (RandomGen g, Monoid w, Monad m) => (Float, Float) -> FastGeneBuilderT g w m Float
gbTypical (min, max) = do
    (dna, gtl) <- get
    let 
        l = geneCount (absoluteGenotype (dna, gtl))
        ml = l `quot` 4
    gbPush (FastGenotype 0 ml)
    mult <- gbNormalizedSum 
    gbPop
    gbPush (FastGenotype ml (l-ml))
    add <- gbNormalizedSum
    gbPop
    return $ min + mult * add * (max - min)


-- | Computation that randomly creates several genes fitting the input range
gbRandomRanges :: (RandomGen g, Monoid w, Monad m) => [(Float, Float)] -> FastGeneBuilderT g w m [Float]
gbRandomRanges ranges = do
    (dna, gtl) <- get
    let 
        rl = length ranges
        gl = geneCount (absoluteGenotype (dna, gtl))
        l = gl `quot` rl
    return $ assert (l > 0) ()
    forM [0..(rl-1)] $ \i -> do
        let 
            (min, max) = ranges !! i
            short = gbNormalizedSum >>= \x -> return $ min + (max-min) * x
            long = gbTypical (min, max)
        gbPush (FastGenotype (i*l) l)
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
--gbRandom :: (RandomGen g, Monoid w, Monad m) => FastGenotype -> GbRandomParams -> FastGeneBuilderT m Float
-}