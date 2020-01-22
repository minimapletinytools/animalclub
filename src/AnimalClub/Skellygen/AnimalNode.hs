{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE BangPatterns #-}

-- TOOD split into files
-- AnimalScriptTypes.hs
-- AnimalProprty.hs
-- AnimalNode.hs

module AnimalClub.Skellygen.AnimalNode (
    BoneTrans(..),
    composeBoneTrans,
    applyBoneTrans,
    BoneName(..),
    BoneName'(..),
    toBoneName',
    AnimalNode(..),
    flipAnimalNode,
) where

import           Control.DeepSeq
import           GHC.Generics                           (Generic)
import           Lens.Micro.Platform                    (makeLenses, set)
--import qualified Data.List as List
--import qualified Data.Map as Map
import qualified Data.Text                              as T

--import qualified Debug.Trace as Debug

import           AnimalClub.Skellygen.Math.Hierarchical
import qualified AnimalClub.Skellygen.Math.TRS          as TRS

import           Linear.V3
--import Linear.Quaternion as Q



-- TODO Bone stuff to be refactored
-- | TODO new comment
-- TODO I don't know if I really like this EnumBone thing...
-- Maybe something like `Bone T.Text [Tags]` where the third parameter is a list of tags (e.g. L/R/F/B/whatever) (or better yet a bitmask)
-- [I don't understand what the comment below means anymore]
-- index is NOT hierarchical
-- you can hack it to be hierarchical by using the nth digit for the nth child or whatever
-- this feature omitted because it's more complicated than it's worth which is not much in most cases
data BoneName =
    Bone T.Text -- ^ vanilla bone
    | EnumBone T.Text Int BoneTrans -- ^ enumerated bone: name index transform
    deriving (Show)

-- | adds ways to match more types of bones
-- TODO is there a more functional way to do this e.g. something like BoneMatcher BoneName -> Bool kinda thing?
data BoneName' =
    Bone' T.Text -- ^ specific bone matching Bone String
    | EnumBone' T.Text Int -- ^ specific bone matching EnumBone String Int
    | AllBones' T.Text -- ^ all bones matching EnumBone String
    | EnumBones' T.Text [Int] -- TODO
    deriving (Show, Ord, Eq, Read, NFData, Generic)

-- TODO rename
toBoneName' :: BoneName -> BoneName'
toBoneName' (Bone name)             = Bone' name
toBoneName' (EnumBone name index _) = EnumBone' name index

-- TODO rename
-- toAll :: BoneName' -> BoneName'
-- toAll (EnumBone' name _) = AllBones' name





-- | user friendly representation of a Bone transformation
-- BoneTrans applies a transformation relative to identity TRS
-- the transformation effects all children
-- so, for example, if you have two legs, you only need to add ReflX at the hips
-- BoneTrans is applied to _trs'/_pos of AnimalNode'/AnimalNode respectively
-- and by extension it also affects _orientation of AnimalProperty
data BoneTrans = Same | ReflX | ReflY | ReflZ | ArbTrans (TRS.TRS Float -> TRS.TRS Float)

instance Show BoneTrans where
    show Same         = "Same"
    show ReflX        = "ReflX"
    show ReflY        = "ReflY"
    show ReflZ        = "ReflZ"
    show (ArbTrans _) = "ArbTrans"

-- | combine two BoneTrans together
composeBoneTrans :: BoneTrans -> BoneTrans -> BoneTrans
composeBoneTrans Same x      = x
composeBoneTrans x Same      = x
composeBoneTrans ReflX ReflX = Same
composeBoneTrans ReflY ReflY = Same
composeBoneTrans ReflZ ReflZ = Same
composeBoneTrans x y         = ArbTrans $ applyBoneTrans x . applyBoneTrans y

-- BoneTrans are not really 'Hierarchical', this is mainly for syntactic convenience
instance Hierarchical BoneTrans where
    inherit = composeBoneTrans

-- | applies BoneTrans to a TRS
applyBoneTrans :: BoneTrans -> TRS.TRS Float -> TRS.TRS Float
applyBoneTrans Same = id
applyBoneTrans ReflX = inherit (set TRS.scale (TRS.makeScale $ V3 (-1) 1 1) TRS.identity)
applyBoneTrans ReflY = inherit (set TRS.scale (TRS.makeScale $ V3 1 (-1) 1) TRS.identity)
applyBoneTrans ReflZ = inherit (set TRS.scale (TRS.makeScale $ V3 1 1 (-1)) TRS.identity)
-- just for testing
--applyBoneTrans ReflZ = (>*>) (set TRS.trans (V3 0 0 1) TRS.identity)
applyBoneTrans (ArbTrans f) = f


-- TODO add optional orientation parameter
-- direction is always looking down bone, orientation determines rotation along that bone
-- except in null bone cases, in which case, I guess you need a second direction parameter :\
-- make it optional so it shoudl look like
--data NodeOrientation = Default | Up (V3 Float) | Full (Quaternion Float)

-- | these define static properties that make up the base SkellyNode
-- user friendly version that is limited in what can be expressed
-- basically, as a bunch of connected points in space
-- orientations are automatically determined based on parent position (see comments in AnimalScript)
-- positions and thickness can be specified as absolute or relative for convenience I guess
-- (albeit this makes things more complicated so consider switching to everything in Abs coordinates
data AnimalNode = AnimalNode {
    _name      :: BoneName, -- ^ name and transformation if relevant
    _pos       :: AbsOrRel (V3 Float), -- ^ position, relative to parent if rel, 'BoneTrans' in 'BoneName' is applied to this
    _thickness :: AbsOrRel Float, -- ^ base thickness, relative to parent thickness if rel
    _isRoot    :: Bool,
    _children  :: [AnimalNode]
    -- TODO some orientation parameter (right now orientation is determined based on parent position)
    -- _nodeOrientation :: NodeOrientation
}

makeLenses ''AnimalNode

-- TODO helpers for constructing AnimalNode
--defAnimalNode = AnimalNode {}
-- makeAnimalNode :: ... -> AnimalNode

-- | flip an AnimalNode, the children of the flipped parent will inherit the flipped parent's new transform
-- but their relative transforms do not change
--
-- N.B., there is currently no way to flip something that has child that has been flipped
-- without munging all the indices rendering the flipped children indistinguishable
-- if we are going to support nested flips, maybe try something this
-- flipAnimalNodeFancy :: BoneTrans -> [Int] -> AnimalNode -> AnimalNode
--
flipAnimalNode ::
    BoneTrans -- ^ trans to flip by (not passed down through children)
    -> Int -- ^ new index applied to all children
    -> AnimalNode -- ^ node to flip
    -> AnimalNode
flipAnimalNode bt i an = set children newChildren $ set name newName an where
    newName = case _name an of
        EnumBone boneName _ bt' -> EnumBone boneName i (composeBoneTrans bt bt')
        _ -> error "can only flip EnumBone"
    newChildren = map (flipAnimalNode Same i) (_children an)
