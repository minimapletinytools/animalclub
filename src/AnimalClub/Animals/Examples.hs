

module AnimalClub.Animals.Examples (
  writeExampleObjs
  , module AnimalClub.Animals.Examples.Goat
) where

import           Relude

import           AnimalClub.Animals.Examples.Goat
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Mesh
import qualified Data.Text.IO                     as T


-- | write the example skeletons
writeExampleObjs :: IO ()
writeExampleObjs = do
    T.writeFile "goat.obj" . potatoMeshToObj . generatePotatoMesh . animalNodeToSkellyNode $ goatAnimalNode
    T.writeFile "worm.obj" . potatoMeshToObj . generatePotatoMesh . animalNodeToSkellyNode $ worm
    T.writeFile "worm2.obj" . potatoMeshToObj . generatePotatoMesh . animalNodeToSkellyNode $ worm2
    T.writeFile "octopus.obj" . potatoMeshToObj . generatePotatoMesh . animalNodeToSkellyNode $ octopusWorm
    T.writeFile "cube.obj" . potatoMeshToObj . generatePotatoMesh . animalNodeToSkellyNode $ cube
