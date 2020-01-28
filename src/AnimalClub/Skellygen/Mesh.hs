{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module AnimalClub.Skellygen.Mesh (
    LocalMesh(..),
    emptyLocalMesh,
    meshToObj,
    transformLocalMesh,
    transformLocalMeshM44,

    CMesh(..),
    toCMesh,

    MutableCMesh(..),
    freezeMutableCMesh,
    thawCMesh

) where

import qualified AnimalClub.Skellygen.TRS     as TRS

import           Control.DeepSeq
import           Control.Monad.Writer.Lazy    (Writer, execWriter, tell)
import           GHC.Generics                 (Generic)

import           AnimalClub.Skellygen.Linear
import           AnimalClub.Skellygen.TRS
import           Data.Int                     (Int32)
import qualified Data.List                    as L
import           Data.Monoid                  (Monoid, mappend)
import           Data.Semigroup               (Semigroup, (<>))
import qualified Data.Vector.Storable         as V
import           Foreign.Storable.Tuple

import           Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as V


type Face = (Int32,Int32,Int32)

-- TODO maybe get rid of this and use CMesh only
data LocalMesh a = LocalMesh ([V3 a], [Face]) deriving (Generic, NFData)

emptyLocalMesh :: LocalMesh a
emptyLocalMesh = LocalMesh ([],[])

map3Tuple :: (a->b) -> (a,a,a) -> (b,b,b)
map3Tuple f (a1,a2,a3) = (f a1, f a2, f a3)

-- | semigroup instance offsets triangle indices appropriately
instance Semigroup (LocalMesh a) where
    (<>) (LocalMesh (m1,i1)) (LocalMesh (m2, i2)) = LocalMesh (m1++m2, i1 ++ map (map3Tuple (+fromIntegral (length m1))) i2)

instance Monoid (LocalMesh a) where
    mempty = LocalMesh ([],[])
    mappend = (<>)

tellV3 :: (Show a) =>  V3 a -> Writer String ()
tellV3 v = do
    tell "v "
    mapM_ (\tv -> tell $ show tv ++ " ") $ v
    tell "\n"

-- TODO change this to Data.Text
meshToObj :: (Show a) => LocalMesh a -> String
meshToObj (LocalMesh m) = execWriter $ do
    tell "#beginning of mesh obj file \ng\n"
    mapM_ tellV3 $ fst m
    mapM_ (\(a1,a2,a3) -> tell $ "f " ++ show (a1+1) ++ " " ++ show (a2+1) ++ " " ++ show (a3+1) ++ "\n") . snd $ m

transformLocalMesh :: (AnimalFloat a) => TRS a -> LocalMesh a -> LocalMesh a
transformLocalMesh trs (LocalMesh (verts, inds)) =  LocalMesh (map mapfn verts, inds) where
    mapfn = mul_TRS_V3 trs

transformLocalMeshM44 :: (AnimalFloat a) => M44 a -> LocalMesh a -> LocalMesh a
transformLocalMeshM44 trs (LocalMesh (verts, inds)) =  LocalMesh (map mapfn verts, inds) where
    mapfn = mul_M44_V3 trs


-- TODO rename this because it can't be used in C directly :(
data CMesh a = CMesh {
  cm_vertices :: V.Vector (V3 a)
  , cm_faces  :: V.Vector Face
}

toCMesh :: (V.Storable a) => LocalMesh a -> CMesh a
toCMesh (LocalMesh (verts, faces)) = CMesh verts' faces' where
  verts' = V.unfoldr L.uncons verts
  faces' = V.unfoldr L.uncons faces




-- prob can delete this
data MutableCMesh s a = MutableCMesh {
  mcm_vertices :: V.MVector s (V3 a)
  , mcm_faces  :: V.MVector s Face
}
freezeMutableCMesh :: (V.Storable a, PrimMonad m) => MutableCMesh (PrimState m) a -> m (CMesh a)
freezeMutableCMesh (MutableCMesh v f) = CMesh <$> V.freeze v <*> V.freeze f
thawCMesh :: (V.Storable a, PrimMonad m) => CMesh a -> m (MutableCMesh (PrimState m) a)
thawCMesh (CMesh v f) = MutableCMesh <$> V.thaw v <*> V.thaw f
