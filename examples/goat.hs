{-

this example uses AnimalClub.Animals.Builder to make a goat

UNFINISH/WIP

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import           ExamplesLib.Skeletons

import           AnimalClub.Animals
import           AnimalClub.Genetics
import           AnimalClub.Skellygen
import           AnimalClub.Skellygen.Mesh
import           AnimalClub.Skellygen.TRS

import           System.Random
import           Text.Printf               (printf)



-- data DepFunc = Linear Float | Mutate Float

-- data ATree = ATree ((Either T.Text SkellyFunc, AutoGeneMethod), [(DepFunc, ATree)])

-- energyNode = ATree ((Left "energy consumption", Normal (0,2) 10), [(Linear 1, undefined)])

gdOrient = (-0.5, 0.5) :: (Float, Float)
gdLength = (0, 2) :: (Float, Float)
gdThick = (0, 2) :: (Float, Float)
--gdOrient = (0,0)
--gdLength = (0,0)
--gdThick = (0,0)


-- NOTE, this is setting thickness on some phantom nodes which is pointless and harmless
goatPropertyList :: [(SkellyFunc Float, AutoGeneMethod Float)]
goatPropertyList =
  sfAutoGenome (WithBoneMatcher (nameFlagMatcher "leg" [BF_Front])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "knee" [BF_Front])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "ankle" [BF_Front])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "toe" [BF_Front])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "leg" [BF_Back])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "knee" [BF_Back])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "ankle" [BF_Back])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneMatcher (nameFlagMatcher "toe" [BF_Back])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneId (BoneId "neck" [])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneId (BoneId "head" [])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneId (BoneId "body" [])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneId (BoneId "body2" [])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneId (BoneId "tailbone" [])) (gdLength,gdThick,gdOrient)
  ++ sfAutoGenome (WithBoneId (BoneId "tailend" [])) (gdLength,gdThick,gdOrient)

main :: IO ()
main = do
    printf "Breeding goats...\n" :: IO ()
    gen <- getStdGen
    let
        goatDNALength = 1000
        goatGenome = makeGenomeFromPropertiesSimple goatDNALength [] goatPropertyList
        original = makeRandDNA gen goatDNALength
        goatProps = generateAnimalProperties (makeBoneIdList goat) $ evalGenome goatGenome original
        skelly = animalNodeToSkellyNodeWithProps goatProps goat
    writeFile "wigglygoat.obj" . meshToObj . generateMesh $ skelly
