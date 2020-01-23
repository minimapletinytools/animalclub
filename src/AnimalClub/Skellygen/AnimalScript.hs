
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE BangPatterns #-}

module AnimalClub.Skellygen.AnimalScript (
    module AnimalClub.Skellygen.AnimalNode,
    module AnimalClub.Skellygen.AnimalProperty,
    animalNodeToSkellyNode,
    animalNodeToSkellyNodeWithProps
) where

import qualified Data.Map                               as Map
import           Lens.Micro.Platform                    (makeLenses, set)
--import qualified Data.Text as T

import           Control.Exception.Base                 (assert)

import qualified Debug.Trace                            as Debug

import           AnimalClub.Skellygen.AnimalNode
import           AnimalClub.Skellygen.AnimalProperty
import           AnimalClub.Skellygen.Math.Hierarchical
import qualified AnimalClub.Skellygen.Math.Quaternion   as QH
import qualified AnimalClub.Skellygen.Math.TRS          as TRS
import qualified AnimalClub.Skellygen.Skellygen         as SN


import           Linear.Metric
import           Linear.Quaternion                      as Q
import           Linear.V3
import           Linear.Vector


-- | these define static properties that make up the base SkellyNode
-- internal, converted from AnimalNode
data AnimalNode' = AnimalNode' {
    -- TODO rename this field
    _name'        :: BoneWT -- ^ name
    , _trsAbs'    :: TRS.TRS Float -- ^ absolute
    , _trs'       :: TRS.TRS Float -- ^ rel to parent
    , _thickness' :: Float -- ^rel to _trans
    , _isRoot'    :: Bool
    , _children'  :: [AnimalNode']
}

makeLenses ''AnimalNode'

-- | sometimes helpful for root node cases
dummyAnimalNode' :: AnimalNode'
dummyAnimalNode' = AnimalNode' (BoneWT (BoneId "" []) Same) TRS.identity TRS.identity 1 True []

-- | convert AnimalNode to internal format FIRST PASS
-- this simply maps
-- this does note apply the BoneTrans in the BoneName yet
_toAnimalNode' ::
    AnimalNode' -- ^ parent Node
    -> AnimalNode -- ^ node to convert
    -> AnimalNode' -- ^ output
_toAnimalNode' pn' cn = outan' where
    p_abs_trs = _trsAbs' pn'
    p_abs_rot = TRS._rot p_abs_trs
    p_abs_rot_inv = QH.inverse p_abs_rot

    c_pos = case _pos cn of
        -- child position is relative to parent translation coordinates assuming parent has identity rotation
        -- so first undo the parent rotation to get the complete relative position
        -- TODO I think this needs to be tested
        Rel a -> Q.rotate p_abs_rot_inv a
        -- Abs a -> TRS.transformV3 (TRS.invTRS p_abs_trs) a -- I have no idea if invTRS works or not
        Abs _ -> error "Absolute positions currently not supported"

    -- TODO process non-existant orientation parameter in AnimalNode
    -- TODO instead of using defaultUp, this should use up vector from parent rotation
    -- convert absolute rotation to rotation relative to parent
    c_rot = QH.lookAtDefaultUp c_pos

    -- put it all together for the final relative trs of the current child node
    c_trs = TRS.TRS c_pos c_rot (TRS.makeScale $ V3 1 1 1)

    --Debug.trace (show (_name cn) ++ ": " ++ show (p_abs_trs >*> c_trs))
    outan' = AnimalNode' {
        _name' = _name cn,
        _trsAbs' = p_abs_trs >*> c_trs,
        _trs' = c_trs,
        _thickness' = case _thickness cn of
            Rel a -> a * _thickness' pn'
            Abs a -> a,
        _isRoot' = _isRoot cn,
        _children' = map (_toAnimalNode' outan') (_children cn)
    }

recomputeAbsTransAnimalNode' ::
    AnimalNode' -- ^ parent node with changed transformation
    -> AnimalNode' -- ^ child node to recompute
    -> AnimalNode' -- ^ recomputed node
recomputeAbsTransAnimalNode' p c = newc where
    newc' = set trsAbs' (_trsAbs' p >*> _trs' c) c
    newc = set children' (map (recomputeAbsTransAnimalNode' newc) (_children' c)) newc'

-- | convert AnimalNode to internal format first pass
-- this function assumes the AnimalNode is in desired base positions
-- then modifies it based on attachOrientation/Distance
-- very inefficient as it needs to recompute the absolute transform of all children everytime it updates a node
-- i.e. o(n^2)
_toAnimalNode'' ::
    AnimalPropertyMap
    -> AnimalNode' -- ^ parent Node
    -> AnimalNode' -- ^ node to convert
    -> AnimalNode' -- ^ output
_toAnimalNode'' props pn cn = outan where
    p_abs_trs = _trsAbs' pn
    p_abs_rot = TRS._rot p_abs_trs
    p_abs_rot_inv = QH.inverse p_abs_rot
    c_rel_trs = _trs' cn
    c_pos = TRS._trans c_rel_trs
    prop = getAnimalProperty (toBoneId $ _name' cn) props

    -- compute new distance
    -- multiplicative distance
    c_pos' = c_pos ^* _distance prop
    -- additive distance (DELETE)
    --bDist = norm c_pos
    --c_pos' = if bDist == 0 then 0 else
    --    c_pos ^* ((bDist + _distance prop) / bDist)

    -- compute new rotation
    --orient = QH.fromEulerXYZ (V3 (pi/6) 0.0 0.0)
    --orient = QH.fromEulerXYZ (V3 0.0 (pi/6) 0.0)
    orient = _orientation prop
    --c_pos'' = Debug.trace (show (_name' cn) ++ show orient) $ Q.rotate (p_abs_rot >*> orient >*> p_abs_rot_inv) c_pos'
    c_pos'' = Q.rotate (p_abs_rot >*> orient >*> p_abs_rot_inv) c_pos'

    -- update with new distance and rotation
    -- TODO double check this is correct in cases where there is funny scale nonsense going on
    c_trs_new = set TRS.rot (QH.lookAtDefaultUp c_pos'') (set TRS.trans c_pos'' c_rel_trs)

    -- TODO at least switch to parMap
    -- inefficient recursion in recursion to update abs trans
    updatedChildren = map (recomputeAbsTransAnimalNode' outan) (_children' cn)
    --updatedChildren = (_children' cn)

    --Debug.trace (show (_name cn) ++ ": " ++ show (p_abs_trs >*> c_trs))
    outan = AnimalNode' {
        -- same as before
        _name' = _name' cn,
        _thickness' = _thickness' cn,
        _isRoot' = _isRoot' cn,
        -- new stuff
        _trs' = c_trs_new,
        _trsAbs' = p_abs_trs >*> c_trs_new,
        _children' = map (_toAnimalNode'' props outan) updatedChildren
    }



reduceBoneTransAnimalNode' ::
    AnimalNode' -- ^ parent node, only necessary because we recompute absTrs for everything
    -> AnimalNode' -- ^ child node being reduced
    -> AnimalNode'
reduceBoneTransAnimalNode' p c = c_new where
    -- apply BoneTrans to c
    p_abs_trs = _trsAbs' p
    BoneWT _ bt = _name' c
    btf = applyBoneTrans bt
    c_rel_trs_new = btf $ _trs' c


    -- TODO copy toAnimalNode'' recursive call, it's cleaner IMO maybe not..
    -- just make it consistent...

    -- update absTrs in all nodes
    -- N.B, this step is not necessary as we currently aren't using absTrs after this point, but we still do it to future proof our data
    -- first set abs and rel trs for current node
    c_new' = set trsAbs' (p_abs_trs >*> c_rel_trs_new) $ set trs' c_rel_trs_new c
    -- then recompute abstrs in children
    c_new'' = set children' (map (recomputeAbsTransAnimalNode' c_new') (_children' c_new')) c_new'
    -- for performance, don't bother doing anything in the Same case
    c_new''' = case _name' c of
        BoneWT _ Same -> c
        _             -> c_new''

    -- then recursively reduce all children
    c_new = set children' (map (reduceBoneTransAnimalNode' c_new''') (_children' c_new''')) c_new'''


-- | convert input AnimalNode to AnimalNode' internal format
-- this function is done very inefficiently for clarity
-- it reconstructs the AnimalNode tree several times to hold intermediate information
-- but you can make this work in a single pass if you make a new class like
-- AnimalNodeBloated that holds all the intermediate information
toAnimalNode' ::
    AnimalPropertyMap
    -> AnimalNode -- ^ top node
    -> AnimalNode' -- ^ output
toAnimalNode' props n = nodes where
    -- first pass, convert AnimalNode to AnimalNode' without any BoneTrans
    nodes' = _toAnimalNode' dummyAnimalNode' n
    -- second pass, update attachOrientation/Distance
    nodes'' = _toAnimalNode'' props dummyAnimalNode' nodes'
    -- third pass, apply BoneTrans
    nodes = reduceBoneTransAnimalNode' dummyAnimalNode' nodes''

-- | convert AnimalNode' to SkellyNode
-- specifically, adds skinning info from AnimalProperty to the AnimalNode
toSkellygen' ::
    AnimalPropertyMap
    -> AnimalNode' -- ^ current node
    -> SN.SkellyNode -- ^ skellygen node for current node
toSkellygen' props cn =  outsn where
    prop = getAnimalProperty (toBoneId (_name' cn)) props
    cn_rel_trs = _trs' cn
    skellyChildren = map (toSkellygen' props) (_children' cn)
    outsn = SN.SkellyNode {
        SN._snDebugName = show (_name' cn),
        SN._snIsRoot = _isRoot' cn,
        SN._snChildren = skellyChildren,
        SN._snTrs = cn_rel_trs,
        --SN._trs = Debug.trace ("rel: " ++ show cn_rel_trs) cn_rel_trs,
        --SN._trs = Debug.trace ("abs: " ++ show (_trsAbs' cn)) cn_rel_trs,
        SN._snThickness = _skinParams prop * _thickness' cn -- combine with base thickness multiplicatively
    }

-- | convert Animal Node to Skellygen
animalNodeToSkellyNode ::
    AnimalNode -- ^ root AnimalNode'
    -> SN.SkellyNode -- ^ root SkellygenNode
animalNodeToSkellyNode = animalNodeToSkellyNodeWithProps Map.empty

animalNodeToSkellyNodeWithProps ::
    AnimalPropertyMap
    -> AnimalNode -- ^ root AnimalNode'
    -> SN.SkellyNode -- ^ root SkellygenNode
animalNodeToSkellyNodeWithProps props an = toSkellygen' props (toAnimalNode' props an)
