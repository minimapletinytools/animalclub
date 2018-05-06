{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


module AnimalClub.Skellygen.AnimalProperty (
    BoneMethod(..),
    SkellyFunc(..),
    AnimalProperty(..),
    AnimalPropertyMap,
    orientation, distance, skinParams,
    lookupBone',
    generateAnimalProperties_
) where

import Control.Exception (assert)
import Control.Lens
import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.List as List

import Linear.V3

import AnimalClub.Skellygen.AnimalNode
import AnimalClub.Skellygen.Math.Hierarchical
import qualified AnimalClub.Skellygen.Math.TRS as TRS
import qualified AnimalClub.Skellygen.Math.Quaternion as QH


import qualified Debug.Trace as Debug
--import Prelude hiding (read)
--import qualified Prelude (read)
--read x = Prelude.read $ Debug.trace x x

data BoneMethod = Orientation | Length | Thickness | Color deriving (Read, Show)

data SkellyFunc = SkellyFunc {
    sfBone' :: BoneName',
    sfMethod :: BoneMethod
} deriving (Read, Show)


-- | these map animal properties used for generating skelly on top of base skelly
-- these are mapped to properties in SkellyNode
data AnimalProperty = AnimalProperty {
    _orientation :: TRS.Rotation Float,
    _distance :: Float,
    _skinParams :: Float
    -- mesh + UV style
    -- UV map properties
    -- texture name, stretch shift,
} deriving (Show, Generic, NFData)

makeLenses ''AnimalProperty

-- | all parameters except name are optional, build on top of this
defaultAnimalProperty :: AnimalProperty
defaultAnimalProperty = AnimalProperty {
    _orientation = QH.identity,
    _distance = 0,
    _skinParams = 1
}

-- |
-- Note it's invalid to have keys with constructor
-- AllBones' or EnumBones', these are used for ADDING to
-- AnimalPropertyMap only
-- TODO create an intermediary type BoneName'' to make this type safe
type AnimalPropertyMap = Map.Map BoneName' AnimalProperty

assertLength :: Int -> [b] -> a -> a
assertLength n xs = assert (length xs == n)

-- this wont work
-- what we want is to be able to specify several bones +

-- | adds properties to a map,
generateAnimalPropertiesInternal_ ::
    AnimalPropertyMap -- ^ accumulating map of properties.
    -> [(SkellyFunc, [Float])] -- ^ list of properties
    -> AnimalPropertyMap -- ^ output accumulated map of properties. EnumBone' property will override AllBone' property
generateAnimalPropertiesInternal_ _props xs = foldl addProp (foldl addProp (foldl addProp  (Map.empty) allProps) enumBonesProps) otherProps where
    -- First go through AllBones' case which will be used as defaults for everything else
    allProps = List.filter ((\case {AllBones' _ -> True; _ -> False}) . sfBone' . fst) xs

    -- Next do multi index EnumBones' case (just convert to EnumBone, inefficient, but whatever)
    enumBonesProps' = List.filter ((\case {EnumBones' _ _ -> True; _ -> False}) . sfBone' . fst) xs
    enumBonesToEnumBoneMapFn :: (SkellyFunc, [Float]) -> [(SkellyFunc, [Float])]
    enumBonesToEnumBoneMapFn = \case
        (SkellyFunc (EnumBones' name indices) method, vals) ->
            map (\i -> (SkellyFunc (EnumBone' name i) method, vals)) indices
        _ -> error "should only be of constructor EnumBones'"
    enumBonesProps = concatMap enumBonesToEnumBoneMapFn enumBonesProps'

    -- Finally do everything else
    otherProps = List.filter ((\case {Bone' _ -> True; EnumBone' _ _ -> True; _ -> False}) . sfBone' . fst) xs

    -- add a proprety to the map
    addProp :: Map.Map BoneName' AnimalProperty -> (SkellyFunc,[Float]) -> Map.Map BoneName' AnimalProperty
    addProp accProp (SkellyFunc boneName method, vals) = Map.insert boneName newProp accProp where
            -- if EnumBone', use AllBone' as default
            defaultProperty = case boneName of
                EnumBone' boneName' _ -> Map.findWithDefault defaultAnimalProperty (AllBones' boneName') accProp
                EnumBones' boneName' _ -> error "this should have been filtered and mapped out!"
                _ -> defaultAnimalProperty
            oldProp = Map.findWithDefault defaultProperty boneName accProp
            newProp = case method of
                Orientation -> assertLength 3 vals $
                    over orientation (inherit $ QH.fromEulerXYZ (V3 (vals !! 0) (vals !! 1) (vals !! 2))) oldProp
                Length -> assertLength 1 vals $
                    over distance (+(vals !! 0)) oldProp
                Thickness -> assertLength 1 vals $
                    over skinParams (+(vals !! 0)) oldProp
                Color -> oldProp

generateAnimalProperties_ ::
    [(SkellyFunc, [Float])] -- ^ list of properties
    -> AnimalPropertyMap -- ^ output accumulated map of properties. EnumBone' property will override AllBone' property
generateAnimalProperties_ = generateAnimalPropertiesInternal_ Map.empty

-- | property access helpers
lookupBone' :: BoneName' -> AnimalPropertyMap -> AnimalProperty
lookupBone' boneName props = Map.findWithDefault defaultProperty boneName props where
    defaultProperty = case boneName of
        EnumBone' boneName' _ -> Map.findWithDefault defaultAnimalProperty (AllBones' boneName') props
        _ -> defaultAnimalProperty
