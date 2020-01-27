

module AnimalClub.Animals.Examples (
  writeExampleObjs
  , module AnimalClub.Animals.Examples.Goat
) where

import           AnimalClub.Animals.Examples.Goat

import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Mesh


-- | write the example skeletons
writeExampleObjs :: IO ()
writeExampleObjs = do
    writeFile "goat.obj" . meshToObj . generateMesh . animalNodeToSkellyNode $ goat
    writeFile "worm.obj" . meshToObj . generateMesh . animalNodeToSkellyNode $ worm
    writeFile "worm2.obj" . meshToObj . generateMesh . animalNodeToSkellyNode $ worm2
