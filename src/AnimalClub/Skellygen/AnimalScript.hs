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
import qualified AnimalClub.Skellygen.Math.Quaternion   as Q
import qualified AnimalClub.Skellygen.Math.TRS          as TRS
import qualified AnimalClub.Skellygen.Skellygen         as SN


import qualified Linear.Matrix                          as M
import           Linear.Metric
import           Linear.Quaternion                      as Q
import           Linear.V3
import           Linear.Vector


-- | these define static properties that make up the base SkellyNode
-- internal, converted from AnimalNode
data AnimalNode' = AnimalNode' {
    -- TODO rename this field
    _name'        :: BoneId -- ^ name
    , _boneTrans' :: BoneTrans
    , _m44Abs'    :: M.M44 Float -- ^ absolute
    , _trs'       :: TRS.TRS Float -- ^ rel to parent
    , _thickness' :: Float -- ^rel to _trans
    , _isRoot'    :: Bool
    , _children'  :: [AnimalNode']
}

makeLenses ''AnimalNode'

-- | sometimes helpful for root node cases
dummyAnimalNode' :: AnimalNode'
dummyAnimalNode' = AnimalNode' (BoneId "" []) Same M.identity TRS.identity 1 True []

-- | converts AnimalNode to internal format superficially
-- i.e. this takes care of converting the '_pos' parameter into the internal '_trs'' and '_m44Abs''
-- as well as converting '_thickness' to the internal relative '_thickness'' format
-- N.B. this does not apply the BoneTrans yet
applyFirstPass ::
    AnimalNode' -- ^ parent Node
    -> AnimalNode -- ^ node to convert
    -> AnimalNode' -- ^ output
applyFirstPass pn' cn = outan' where
    p_abs_m44 = _m44Abs' pn'
    p_abs_m44_inv = M.inv44 p_abs_m44
    --p_abs_rot = TRS._rot p_abs_m44
    --p_abs_rot_inv = Q.inverse p_abs_rot

    c_rel_pos = case _pos cn of
        -- N.B. Originally I had it was only modified by the rotation component but this seems to work fine
        -- this equation is funny :D. I guess applying M44 to V3 is not distributive or something like that
        Rel a -> TRS.mul_M44_V3 p_abs_m44_inv $ TRS.mul_M44_V3 p_abs_m44 (V3 0 0 0) + a
        -- TODO this should be ok, enable and test
        --Abs a -> TRS.mul_M44_V3 p_abs_m44_inv a
        Abs _ -> error "Absolute positions currently not supported"

    -- FUTURE process non-existant orientation parameter in AnimalNode
    -- TODO instead of using defaultUp, this should use up vector from parent rotation
    -- convert absolute rotation to rotation relative to parent
    c_rel_rot = Q.lookAtDefaultUp c_rel_pos

    -- put it all together for the final relative trs of the current child node
    c_trs = TRS.TRS c_rel_pos c_rel_rot (TRS.makeScale $ V3 1 1 1)

    outan' = AnimalNode' {
        _name' = _name cn,
        _boneTrans' = _boneTrans cn,
        _m44Abs' = p_abs_m44 M.!*! TRS.toM44 c_trs,
        _trs' = c_trs,
        _thickness' = case _thickness cn of
            Rel a -> a * _thickness' pn'
            Abs a -> a,
        _isRoot' = _isRoot cn,
        _children' = map (applyFirstPass outan') (_children cn)
    }

-- | this updates the '_trsAbs'' parameter of all children after parent node was updated
update_m44Abs ::
    AnimalNode' -- ^ parent node with changed transformation
    -> AnimalNode' -- ^ child node to recompute
    -> AnimalNode' -- ^ recomputed node
update_m44Abs p c = newc where
    newc' = set m44Abs' (_m44Abs' p M.!*! TRS.toM44 (_trs' c)) c
    newc = set children' (map (update_m44Abs newc) (_children' c)) newc'

-- | applies 'AnimalPropertyMap'
-- this function assumes the AnimalNode is in its starting positions
-- then modifies it based on properties in the given 'AnimalPropertyMap'
-- this is very inefficient as it needs to recompute the absolute transform of all children everytime it updates any node i.e. o(n^2)
applyAnimalPropertyMap ::
    AnimalPropertyMap
    -> AnimalNode' -- ^ parent Node
    -> AnimalNode' -- ^ node to convert
    -> AnimalNode' -- ^ output
applyAnimalPropertyMap props pn cn = outan where
    p_abs_m44 = _m44Abs' pn
    p_abs_m44_inv = M.inv44 p_abs_m44

    --p_abs_rot = TRS._rot p_abs_trs
    --p_abs_rot_inv = Q.inverse p_abs_rot

    c_rel_trs = _trs' cn
    c_rel_pos = TRS._trans c_rel_trs
    prop = getAnimalProperty (_name' cn) props

    -- compute new distance
    -- multiplicative distance
    c_rel_pos' = c_rel_pos ^* _distance prop

    -- additive distance (DELETE)
    --bDist = norm c_res_pos
    --c_res_pos' = if bDist == 0 then 0 else
    --    c_rel_pos ^* ((bDist + _distance prop) / bDist)

    -- compute new rotation
    --orient = Q.fromEulerXYZ (V3 (pi/6) 0.0 0.0)
    --orient = Q.fromEulerXYZ (V3 0.0 (pi/6) 0.0)
    orient = _orientation prop

    -- TODO originally we only did rotation component, not sure what this new version looks like
    --c_rel_pos'' = TRS.mul_M44_V3 (p_abs_m44 M.!*! Q.toM44 orient M.!*! p_abs_m44_inv) c_rel_pos'
    c_rel_pos'' = Q.rotate orient c_rel_pos'
    --c_rel_pos'' =  c_rel_pos'

    -- update with new distance and rotation
    c_rel_trs_new = set TRS.rot (Q.lookAtDefaultUp c_rel_pos'') (set TRS.trans c_rel_pos'' c_rel_trs)

    -- TODO at least switch to parMap
    -- inefficient recursion in recursion to update abs trans
    updatedChildren = map (update_m44Abs outan) (_children' cn)
    --updatedChildren = (_children' cn)

    --Debug.trace (show (_name cn) ++ ": " ++ show (p_abs_trs >*> c_trs))
    outan = AnimalNode' {
        -- same as before
        _name' = _name' cn,
        _boneTrans' = _boneTrans' cn,
        _thickness' = _thickness' cn,
        _isRoot' = _isRoot' cn,
        -- new stuff
        _trs' = c_rel_trs_new,
        _m44Abs' = p_abs_m44 M.!*! TRS.toM44 c_rel_trs_new,
        _children' = map (applyAnimalPropertyMap props outan) updatedChildren
    }

-- | 'AnimalNode' conversion FINAL PASS
-- updates 'AnimalNode'' using the '_boneTrans'' inside it
-- N.B. there's nothing inside of 'AnimalNode'' tracking whether 'BoneTrans'' has been applied or not
-- do not call this function twice!
reduceBoneTrans ::
    AnimalNode' -- ^ parent node, only necessary because we recompute absTrs for everything
    -> AnimalNode' -- ^ child node being reduced
    -> AnimalNode'
reduceBoneTrans p c = c_new where
    -- apply BoneTrans to c
    p_abs_m44 = _m44Abs' p
    bt = _boneTrans' c
    btf = applyBoneTrans bt
    c_rel_trs_new = btf $ _trs' c

    -- TODO copy toAnimalNode'' recursive call, it's cleaner IMO maybe not..
    -- just make it consistent...

    -- update absTrs in all nodes
    -- N.B, this step is not necessary as we currently aren't using absTrs after this point, but we still do it to future proof our data
    -- first set abs and rel trs for current node
    c_new' = set m44Abs' (p_abs_m44 M.!*! TRS.toM44 c_rel_trs_new) $ set trs' c_rel_trs_new c
    -- then recompute abstrs in children
    c_new'' = set children' (map (update_m44Abs c_new') (_children' c_new')) c_new'
    -- for performance, don't bother doing anything in the Same case
    c_new''' = case _boneTrans' c of
        Same -> c
        _    -> c_new''

    -- then recursively reduce all children
    c_new = set children' (map (reduceBoneTrans c_new''') (_children' c_new''')) c_new'''


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
    nodes' = applyFirstPass dummyAnimalNode' n
    -- second pass, update attachOrientation/Distance
    nodes'' = applyAnimalPropertyMap props dummyAnimalNode' nodes'
    -- third pass, apply BoneTrans
    nodes = reduceBoneTrans dummyAnimalNode' nodes''

-- | convert AnimalNode' to SkellyNode
-- specifically, adds skinning info from AnimalProperty to the AnimalNode
toSkellyNode ::
    AnimalPropertyMap
    -> AnimalNode' -- ^ current node
    -> SN.SkellyNode -- ^ skellygen node for current node
toSkellyNode props cn =  outsn where
    prop = getAnimalProperty (_name' cn) props
    cn_rel_trs = _trs' cn
    skellyChildren = map (toSkellyNode props) (_children' cn)
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
animalNodeToSkellyNodeWithProps props an = toSkellyNode props (toAnimalNode' props an)
