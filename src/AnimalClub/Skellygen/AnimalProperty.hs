{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}


module AnimalClub.Skellygen.AnimalProperty (
    BoneMethod(..),
    defThickness, defLength, defOrientation, defColor,
    SkellyFunc(..),
    addValuesToBoneMethod, addValuesToSkellyFunc,
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

-- TODO finish 'Color'
-- |
-- There are no defined overwrite rules when using TLOCombined
-- so do not use it together with Thickness Length and Orientation
-- or you will not be guaranteed which one overwrites which
-- FUTURE for performance, you could add `TLOCombined (Float, Float, TRS.Rotation)`
data BoneMethod = Thickness Float |  Length Float | Orientation (TRS.Rotation Float) | Color () deriving (Read, Show, Generic, NFData)

defThickness :: BoneMethod
defThickness = Thickness 1
defLength :: BoneMethod
defLength = Length 1
defOrientation :: BoneMethod
defOrientation = Orientation QH.identity
defColor :: BoneMethod
defColor = Color ()

-- |
-- SkellyFunc represents a method applied to a bone
-- parameters to the method are passed in as [Float]
-- such non-type safety 😱
data SkellyFunc = SkellyFunc {
    sfBone'  :: BoneName',
    sfMethod :: BoneMethod
} deriving (Read, Show, Generic, NFData)

-- | adds values to parameters in BoneMethod
-- N.B this does no error checking on length of list being passed in
addValuesToBoneMethod :: BoneMethod -> [Float] -> BoneMethod
addValuesToBoneMethod m vals = case m of
  Orientation x ->
    Orientation $ x `inherit` QH.fromEulerXYZ (V3 (vals !! 0) (vals !! 1) (vals !! 2))
  Length x ->
    Length $ x * (vals !! 0)
  Thickness x ->
    Thickness $ x * (vals !! 0)
  Color x -> Color x

-- | adds values to parameters in BoneMethod inside SkellyFunc
-- N.B this does no error checking on length of list being passed in
addValuesToSkellyFunc :: SkellyFunc -> [Float] -> SkellyFunc
addValuesToSkellyFunc (SkellyFunc b m) vals = SkellyFunc b (addValuesToBoneMethod m vals)



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
    -> [SkellyFunc] -- ^ list of properties
    -> AnimalPropertyMap -- ^ output accumulated map of properties. EnumBone' property will override AllBone' property
generateAnimalPropertiesInternal_ _props xs = foldl addProp (foldl addProp (foldl addProp Map.empty allProps) enumBonesProps) otherProps where
    -- First go through AllBones' case which will be used as defaults for everything else
    allProps = List.filter ((\case {AllBones' _ -> True; _ -> False}) . sfBone') xs

    -- Next do multi index EnumBones' case (just convert to EnumBone, inefficient, but whatever)
    -- UNTESTED
    enumBonesProps' = List.filter ((\case {EnumBones' _ _ -> True; _ -> False}) . sfBone') xs
    enumBonesToEnumBoneMapFn = \case
        (SkellyFunc (EnumBones' name indices) method) ->
            map (\i -> (SkellyFunc (EnumBone' name i) method)) indices
        _ -> error "should only be of constructor EnumBones'"
    enumBonesProps = concatMap enumBonesToEnumBoneMapFn enumBonesProps'

    -- Finally do everything else
    otherProps = List.filter ((\case {Bone' _ -> True; EnumBone' _ _ -> True; _ -> False}) . sfBone') xs

    -- add a proprety to the map
    addProp :: Map.Map BoneName' AnimalProperty -> (SkellyFunc) -> Map.Map BoneName' AnimalProperty
    addProp accProp (SkellyFunc boneName method) = Map.insert boneName newProp accProp where
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
                Orientation x ->
                    over orientation (inherit x) oldProp
                    --over orientation (inherit $ QH.fromEulerXYZ (V3 (vals !! 0) (vals !! 1) (vals !! 2))) oldProp
                Length x ->
                    over distance (*x) oldProp
                    --over distance (*(vals !! 0)) oldProp
                Thickness x ->
                    over skinParams (*x) oldProp
                    --over skinParams (*(vals !! 0)) oldProp
                -- TODO
                Color _ -> oldProp


generateAnimalProperties_ ::
    [SkellyFunc] -- ^ list of properties
    -> AnimalPropertyMap -- ^ output accumulated map of properties. EnumBone' property will override AllBone' property
generateAnimalProperties_ = generateAnimalPropertiesInternal_ Map.empty

-- | property access helpers
lookupBone' :: BoneName' -> AnimalPropertyMap -> AnimalProperty
lookupBone' boneName props = Map.findWithDefault defaultProperty boneName props where
    defaultProperty = case boneName of
        EnumBone' boneName' _ -> Map.findWithDefault defaultAnimalProperty (AllBones' boneName') props
        _ -> defaultAnimalProperty
