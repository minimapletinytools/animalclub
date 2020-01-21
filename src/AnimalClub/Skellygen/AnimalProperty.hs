{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}


module AnimalClub.Skellygen.AnimalProperty (
    BoneMethod(..),
    SkellyFunc(..),
    AnimalProperty(..),
    AnimalPropertyMap,
    orientation, distance, skinParams,
    lookupBone',
    generateAnimalProperties_
) where

import           Control.DeepSeq
import           Control.Exception                      (assert)
import qualified Data.List                              as List
import qualified Data.Map                               as Map
import qualified Data.Text                              as T
import           GHC.Generics                           (Generic)
import           Lens.Micro.Platform

import           Linear.V3

import           AnimalClub.Skellygen.AnimalNode
import           AnimalClub.Skellygen.Math.Hierarchical
import qualified AnimalClub.Skellygen.Math.Quaternion   as QH
import qualified AnimalClub.Skellygen.Math.TRS          as TRS


import qualified Debug.Trace                            as Debug
--import Prelude hiding (read)
--import qualified Prelude (read)
--read x = Prelude.read $ Debug.trace x x

-- |
-- There are no defined overwrite rules when using TLOCombined
-- so do not use it together with Thickness Length and Orientation
-- or you will not be guaranteed which one overwrites which
data BoneMethod = Thickness |  Length | Orientation | TLOCombined | Color deriving (Read, Show, Generic, NFData)

-- TODO add parameters to this so we don't use [Float] anymore!!
-- |
-- SkellyFunc represents a method applied to a bone
-- parameters to the method are passed in as [Float]
-- such non-type safety 😱
data SkellyFunc = SkellyFunc {
    sfBone'  :: BoneName',
    sfMethod :: BoneMethod
} deriving (Read, Show, Generic, NFData)


-- | used for generating skelly over each bone of the base skelly
-- these are mapped to properties in SkellyNode
data AnimalProperty = AnimalProperty {
    _orientation :: TRS.Rotation Float, -- ^ combines multiplicatively
    _distance    :: Float, -- ^ combines multiplicatively
    _skinParams  :: Float -- ^ combines multiplicatively
    -- mesh + UV style
    -- UV map properties
    -- texture name, stretch shift,
} deriving (Show, Generic, NFData)

makeLenses ''AnimalProperty

-- | the identity AnimalProperty
defaultAnimalProperty :: AnimalProperty
defaultAnimalProperty = AnimalProperty {
    _orientation = QH.identity,
    _distance = 1,
    _skinParams = 1
}

-- | TODO
-- Note it's invalid to have keys with constructor
-- AllBones' or EnumBones', these are used for ADDING to
-- AnimalPropertyMap only
-- TODO create an intermediary type BoneName'' to make this type safe
type AnimalPropertyMap = Map.Map BoneName' AnimalProperty

-- |
-- TODO this assert is not actually getting evaluated
assertLength :: Int -> [b] -> a -> a
assertLength n xs = assert (length xs == n)

-- this wont work
-- what we want is to be able to specify several bones +

-- TODO get rid of EnumBones' nonsense probably?
-- | adds properties to a map,
generateAnimalPropertiesInternal_ ::
    AnimalPropertyMap -- ^ accumulating map of properties.
    -> [(SkellyFunc, [Float])] -- ^ list of properties
    -> AnimalPropertyMap -- ^ output accumulated map of properties. EnumBone' property will override AllBone' property
generateAnimalPropertiesInternal_ _props xs = foldl addProp (foldl addProp (foldl addProp Map.empty allProps) enumBonesProps) otherProps where
    -- First go through AllBones' case which will be used as defaults for everything else
    allProps = List.filter ((\case {AllBones' _ -> True; _ -> False}) . sfBone' . fst) xs

    -- Next do multi index EnumBones' case (just convert to EnumBone, inefficient, but whatever)
    -- UNTESTED
    enumBonesProps' = List.filter ((\case {EnumBones' _ _ -> True; _ -> False}) . sfBone' . fst) xs
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
                EnumBones' boneName' _ -> error $ "found " ++ (show boneName') ++ ", this should have been filtered and mapped out!"
                _ -> defaultAnimalProperty
            oldProp = Map.findWithDefault defaultProperty boneName accProp
            -- combine with what's already there
            -- TODO consider making combine/not combine a parameter
            --newProp = Debug.trace (show method ++ " " ++ show vals) $ case method of
            newProp = case method of
                Orientation -> assertLength 3 vals $
                    over orientation (inherit $ QH.fromEulerXYZ (V3 (vals !! 0) (vals !! 1) (vals !! 2))) oldProp
                Length -> assertLength 1 vals $
                    over distance (*(vals !! 0)) oldProp
                Thickness -> assertLength 1 vals $
                    over skinParams (*(vals !! 0)) oldProp
                TLOCombined -> assertLength 5 vals $
                    over orientation (inherit $ QH.fromEulerXYZ (V3 (vals !! 2) (vals !! 3) (vals !! 4)))
                    $ over distance (*(vals !! 1))
                    $ over skinParams (*(vals !! 0)) oldProp
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
