{-|
Module      : Genetics
Description :
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

Includes all modules exposed by genetics
-}

module AnimalClub.Genetics (
    module AnimalClub.Genetics.DNA,
    module AnimalClub.Genetics.Gene,
    --module AnimalClub.Genetics.Genotype
    module AnimalClub.Genetics.ArtisinalFreeRangeGenotype
) where

import AnimalClub.Genetics.DNA
import AnimalClub.Genetics.Gene
--import AnimalClub.Genetics.Genotype
import AnimalClub.Genetics.ArtisinalFreeRangeGenotype
