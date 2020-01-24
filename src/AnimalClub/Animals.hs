{-|
Module      : Animals
Description :
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

basic program flow for creating animals:

1. create a tree of 'AnimalNode's using 'BoneId's to define initial skeleton for your animal
  - use 'BoneTrans' to create symmetry in your definition
2. extract list of 'BoneId's from AnimalNode using 'makeBoneIdList'
3. generate a list of 'SkellyFunc's referencing bones in step 2.
4. convert lists from step 2. and 3. into 'AnimalPropertyMap' using 'generateAnimalProperties'
5. apply properties from step 4. to initial skeleton in step 1. using 'animalNodeToSkellyNodeWithProps'

-}

module AnimalClub.Animals (
  module AnimalClub.Animals.Animal,
  module AnimalClub.Animals.Builder
) where

import AnimalClub.Animals.Animal
import AnimalClub.Animals.Builder
