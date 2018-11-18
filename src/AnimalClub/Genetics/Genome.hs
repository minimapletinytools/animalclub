{-|
Module      : Genome
Description : Genotype with DNA
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

TODO add GenomeT versions

-}

module AnimalClub.Genetics.Genome (
    Genome(..),
    evalGenome
) where

import AnimalClub.Genetics.DNA
import AnimalClub.Genetics.Genotype


-- | Genome packages Genotype with its expected DNA length and random generator
-- parameters are: DNA length, builder, random gen to seed builder
data Genome g w = Genome Int (Genotype g w ()) g


-- | evalute the genome and obtain its output
evalGenome :: Genome g w -> DNA -> w
evalGenome (Genome _ gb g) dna = evalGeneBuilder gb dna g
