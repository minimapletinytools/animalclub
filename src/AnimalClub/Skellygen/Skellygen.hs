{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module AnimalClub.Skellygen.Skellygen
    ( SkellyNode(..)
    , generateMesh
    ) where

import           Control.DeepSeq
import           GHC.Generics              (Generic)
import           Lens.Micro.Platform
import qualified Linear.Matrix             as M
import           Linear.Metric
import           Linear.Quaternion         (rotate)
import           Linear.V3
import           Linear.Vector


import           AnimalClub.Skellygen.Mesh
import           AnimalClub.Skellygen.TRS

import qualified Debug.Trace               as Debug

-- |
-- prefixed names due to unfortunate naming conflict with AnimalNode
data SkellyNode a = SkellyNode
    {
    _snDebugName   :: String
    , _snIsRoot    :: Bool
    , _snChildren  :: [SkellyNode a]
    , _snTrs       :: TRS a -- ^ relative to parent
    , _snThickness :: a -- ^ base physical size of joint.
    } deriving (Show, Generic, NFData)

--dummyParent :: SkellyNode
--dummyParent = SkellyNode True [] identity rotationIdentity 0.0 1.0
makeLenses ''SkellyNode

data BoxSkinParameters a = BoxSkinParameters
    { extension :: (a, a) --how much box sticks out of each end (parent, node)
    , boxSize   :: (a, a) --size of box at each joint (parent, node)
    } deriving (Show)

defaultBoxParam :: BoxSkinParameters Float
defaultBoxParam = BoxSkinParameters (0.005, 0.005) (0.005, 0.005)

_normalize :: (TRSFloating a) => V3 a -> V3 a
_normalize v = (1 / norm v) *^ v

-- TODO it's better to write this function where it takes a thickness square at the origin facing neutral and apply the transformation to it
generateSingleMeshLocal ::
    (TRSFloating a)
    => TRS a -- ^ input node transform
    -> a -- ^ input thickness
    -> a -- ^ node parent thickness
    -> Mesh a -- ^ output mesh
generateSingleMeshLocal pos ct pt =
  if length' < 1e-6
    then Mesh ([], [])
    else Mesh (startPoints ++ endPoints, sides ++ caps)
  where
    start' = V3 0 0 0
    end' = _trans pos
    length' = norm (end' - start')
    normalized = _normalize $ end' - start'
    start = start' --  - ex *^ normalized
    end = end' -- + ey *^ normalized
    -- TODO normalAxis should use the up direction of pos
    --normalAxis = Debug.trace (show $ fromTo (V3 0 1 0) normalized) $ rotate (fromTo (V3 0 1 0) normalized)
    normalAxis = rotate (fromTo (V3 0 1 0) normalized)
    startPoints = map mapfn [i * pi / 2.0 | i <- [0,1,2,3]] where
      mapfn a = start ^+^ normalAxis npt where
        npt = V3 (pt * cos a) 0 (pt * sin a)
    endPoints = map mapfn [i * pi / 2.0 | i <- [0,1,2,3]] where
      mapfn a = end ^+^ normalAxis npt where
        npt = V3 (ct * cos a) 0 (ct * sin a)


    sides = [0, 4, 1, 1, 4, 5, 1, 5, 2, 2, 5, 6, 2, 6, 7, 3, 2, 7, 3, 7, 0, 0, 7, 4]
    caps = [0, 1, 2, 2, 3, 0, 4, 5, 6, 6, 7, 4]

_generateMesh ::
    (TRSFloating a)
    => M.M44 a -- ^ parent ABS transform
    -> a -- ^ parent thickness
    -> SkellyNode a -- ^ node to generate
    -> Mesh a -- ^ output mesh
_generateMesh p_snM44 p_thick skn = selfMesh `mappend` mconcat cmeshes
  where
    thick = _snThickness skn
    reltrs = _snTrs skn
    --selfMesh = Debug.trace ("skn: " ++ (show (_snDebugName skn)) ++ " p: " ++ show (_trans p_snTrs) ++ " c: " ++ show (_trans reltrs)) $
    --selfMesh = Debug.trace ("sknabs: " ++ show abstrs ++ " p: " ++ show (_rot p_snTrs) ++ " c: " ++ show (_rot reltrs)) $
    selfMesh =
        if _snIsRoot skn then emptyMesh else transformMeshM44 p_snM44 $ generateSingleMeshLocal reltrs thick p_thick
    -- TODO change this to M44 multiplication
    absM44 = p_snM44 M.!*! conv_TRS_M44 reltrs
    cmeshes = map (_generateMesh absM44 thick) (_snChildren skn)

generateMesh ::
    (TRSFloating a)
    => SkellyNode a -- ^ input top level parent node
    -> Mesh a -- ^ output mesh
generateMesh skn = _generateMesh M.identity 1.0 skn
