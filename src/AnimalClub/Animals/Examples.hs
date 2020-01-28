

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
    writeFile "goat.obj" . meshToObj . generateLocalMesh . animalNodeToSkellyNode $ goatAnimalNode
    writeFile "worm.obj" . meshToObj . generateLocalMesh . animalNodeToSkellyNode $ worm
    writeFile "worm2.obj" . meshToObj . generateLocalMesh . animalNodeToSkellyNode $ worm2
