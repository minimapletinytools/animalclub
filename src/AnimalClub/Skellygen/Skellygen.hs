{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module AnimalClub.Skellygen.Skellygen
  ( SkellyNode(..)
  , generateLocalMesh
  ) where

import           Control.DeepSeq
import           GHC.Generics
import           Lens.Micro.Platform

import qualified Data.Vector.Storable.Mutable as MV

import           AnimalClub.Skellygen.Linear
import           AnimalClub.Skellygen.Mesh
import           AnimalClub.Skellygen.TRS

import qualified Debug.Trace                  as Debug



{-
  cube indexing for 'generateSingleLocalMesh' and 'generateSinglePotatoMesh'
  coordinates interpreted looking in direction of limb (from parent to child)


          start
          0-------1
        / |     / |
       3-------2  |
       |  4----|--5
       | /     | /
       7-------6
           end

           y
           | z
           |/
       x---/

  (actually the current implementation is offset by 45 degrees)

  uv mapping of one face
  (0,0)   (1,0)
  0-------1
  |       |
  |       |
  3-------2
  (0,1)   (1,1)
-}



-- |
-- prefixed names due to unfortunate naming conflict with AnimalNode
data SkellyNode a = SkellyNode
  {
  _snDebugName   :: String
  , _snIsPhantom :: Bool
  , _snChildren  :: [SkellyNode a]
  , _snM44Rel    :: M44 a -- ^ relative to parent
  , _snThickness :: a -- ^ base physical size of joint.
  } deriving (Show, Generic, NFData)

--dummyParent :: SkellyNode
--dummyParent = SkellyNode True [] identityTRS identityRotation 0.0 1.0
makeLenses ''SkellyNode

data BoxSkinParameters a = BoxSkinParameters
  { extension :: (a, a) --how much box sticks out of each end (parent, node)
  , boxSize   :: (a, a) --size of box at each joint (parent, node)
  } deriving (Show)

defaultBoxParam :: (AnimalFloat a) => BoxSkinParameters a
defaultBoxParam = BoxSkinParameters (0.005, 0.005) (0.005, 0.005)

_normalize :: (AnimalFloat a) => V3 a -> V3 a
_normalize v = (1 / norm v) *^ v

-- TODO it's better to write this function where it takes a thickness square at the origin facing neutral and apply the transformation to it
generateSingleLocalMesh ::
  (AnimalFloat a)
  => M44 a -- ^ input node transform
  -> a -- ^ input thickness
  -> a -- ^ node parent thickness
  -> LocalMesh a -- ^ output mesh
generateSingleLocalMesh pos ct pt =
 if length' < 1e-6
  then emptyLocalMesh
  else LocalMesh (startPoints ++ endPoints, sides ++ caps)
 where
  end' = view translation pos
  start' = V3 0 0 0
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

  sides = [(0, 1, 4), (5, 4, 1), (1, 2, 5), (6, 5, 2), (2, 3, 6), (7, 6, 3), (3, 0, 7), (4, 7, 0)]
  caps = [(0, 1, 3), (2, 3, 1), (6, 7, 5), (4, 5, 7)]

-- same as above but formats different and adds normals + uvs
generateSinglePotatoMesh ::
  (AnimalFloat a)
  => M44 a -- ^ input node transform
  -> a -- ^ input thickness
  -> a -- ^ node parent thickness
  -> PotatoMesh a -- ^ output mesh
generateSinglePotatoMesh pos ct pt =
 if length' < 1e-6
  then emptyPotatoMesh
  else r
 where
  end' = view translation pos
  start' = V3 0 0 0
  length' = norm (end' - start')
  normalized = _normalize $ end' - start'
  start = start' --  - ex *^ normalized
  end = end' -- + ey *^ normalized

  -- TODO normalAxis should use the up direction of pos
  normalAxis = rotate (fromTo (V3 0 1 0) normalized)

  startPoints = map mapfn [i * pi / 2.0 | i <- [0,1,2,3]] where
   mapfn a = start ^+^ normalAxis npt where
    npt = V3 (pt * cos a) 0 (pt * sin a)

  endPoints = map mapfn [i * pi / 2.0 | i <- [0,1,2,3]] where
   mapfn a = end ^+^ normalAxis npt where
    npt = V3 (ct * cos a) 0 (ct * sin a)

  allPoints = startPoints ++ endPoints

  sides = [(0, 1, 4), (5, 4, 1), (1, 2, 5), (6, 5, 2), (2, 3, 6), (7, 6, 3), (3, 0, 7), (4, 7, 0)]
  caps = [(0, 1, 3), (2, 3, 1), (6, 7, 5), (4, 5, 7)]
  allIndices = sides ++ caps

  sideNormals = [V3 0 0 1, V3 0 0 1, V3 (-1) 0 0, V3 (-1) 0 0, V3 1 0 0, V3 1 0 0, V3 1 0 0, V3 1 0 0]
  capNormals = [V3 0 1 0, V3 0 1 0, V3 0 (-1) 0, V3 0 (-1) 0]
  allNormals = sideNormals ++ capNormals

  -- rendy requires same buffer indices for position, normal and tex coords
  -- therefore we reindex everything and duplicate positions/normals
  p = map (\(a,b,c) -> [(allPoints !! a), (allPoints !!b), (allPoints !!c)]) allIndices
  n = map (\(a,b,c) -> [(allNormals !! a), (allNormals !!b), (allNormals !!c)]) allIndices
  tc = take 6 . repeat $ [V2 0 0 , V2 1 0, V2 0 1, V2 1 1, V2 0 1, V2 1 0]
  i = [(x+0, x+1, x+2)| y <- [0..11], let x = y*3]
  r = PotatoMesh {
      positions = mconcat p
      , normals = mconcat n
      , texCoords = mconcat tc
      , indices = i
    }

_generateLocalMesh ::
  (AnimalFloat a)
  => M44 a -- ^ parent ABS transform
  -> a -- ^ parent thickness
  -> SkellyNode a -- ^ node to generate
  -> LocalMesh a -- ^ output mesh
_generateLocalMesh p_snM44 p_thick skn = selfLocalMesh <> mconcat cmeshes where
 thick = _snThickness skn
 relm44 = _snM44Rel skn
 --selfLocalMesh = Debug.trace ("skn: " ++ (show (_snDebugName skn)) ++ " p: " ++ show (_trans p_snTrs) ++ " c: " ++ show (_trans reltrs)) $
 --selfLocalMesh = Debug.trace ("sknabs: " ++ show abstrs ++ " p: " ++ show (_rot p_snTrs) ++ " c: " ++ show (_rot reltrs)) $
 selfLocalMesh = if _snIsPhantom skn
  then emptyLocalMesh
  else transformLocalMeshM44 p_snM44 $ generateSingleLocalMesh relm44 thick p_thick
 absM44 = p_snM44 !*! relm44
 cmeshes = map (_generateLocalMesh absM44 thick) (_snChildren skn)


generateLocalMesh ::
  (AnimalFloat a)
  => SkellyNode a -- ^ input top level parent node
  -> LocalMesh a -- ^ output mesh
generateLocalMesh skn = _generateLocalMesh identity 1.0 skn
