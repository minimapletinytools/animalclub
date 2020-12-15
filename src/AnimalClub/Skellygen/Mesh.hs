{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module AnimalClub.Skellygen.Mesh (

  PotatoMesh(..),
  emptyPotatoMesh,
  potatoMeshToObj,
  transformPotatoMeshM44,
  concatPotatoMesh,

  -- old stuff prob can delete
  LocalMesh(..),
  meshToObj,
  transformLocalMesh,
  transformLocalMeshM44,
  CMesh(..),
  toCMesh,

  MutableCMesh(..),
  freezeMutableCMesh,
  thawCMesh

) where

import           Relude                      hiding (transpose)

import           Control.Monad.Writer.Lazy   (Writer, execWriter, tell)
import           GHC.Generics                (Generic)

import           AnimalClub.Skellygen.Linear
import           AnimalClub.Skellygen.TRS
import           Data.Int                    (Int32)
import           Data.Monoid                 (Monoid, mappend)
import           Data.Semigroup              (Semigroup, (<>))
import qualified Data.Text                   as T
import qualified Data.Vector.Storable        as V
import           Foreign.Storable.Tuple      ()

import           Control.Monad.Primitive


type Face = (Int32,Int32,Int32)

-- indices index all the other data in this representation
data PotatoMesh a = PotatoMesh {
  positions   :: V.Vector (V3 a)
  , normals   :: V.Vector (V3 a)
  , texCoords :: V.Vector (V2 a)
  , indices   :: V.Vector Face
} deriving (Generic, NFData, Show)

emptyPotatoMesh :: (AnimalFloat a) => PotatoMesh a
emptyPotatoMesh = PotatoMesh V.empty V.empty V.empty V.empty

-- this can probably be improved even more to take advantage of stream fusion
concatPotatoMesh :: (AnimalFloat a) => [PotatoMesh a] -> PotatoMesh a
concatPotatoMesh pms = r where
  foldfn (pl, nl, tcl, il, offset) (PotatoMesh p n tc i) =
    (pl++[p], nl++[n], tcl++[tc], il++[offseti], newOffset) where
      offseti = V.map (map3Tuple (+ fromIntegral offset)) i
      newOffset = offset + V.length p
  (plf,nlf,tclf,ilf,_) = foldl' foldfn ([],[],[],[],0) pms
  r = PotatoMesh {
      positions = V.concat plf
      , normals = V.concat nlf
      , texCoords = V.concat tclf
      , indices = V.concat ilf
    }

map3Tuple :: (a->b) -> (a,a,a) -> (b,b,b)
map3Tuple f (a1,a2,a3) = (f a1, f a2, f a3)

showText :: Show a => a -> T.Text
showText = T.pack . show

tellV :: (Show a, Traversable t) => T.Text -> t a -> Writer T.Text ()
tellV key v = do
  tell (key <> " ")
  mapM_ (\tv -> tell $ showText tv <> " ") $ v
  tell "\n"

potatoMeshToObj :: (AnimalFloat a) => PotatoMesh a -> T.Text
potatoMeshToObj (PotatoMesh p n tc i) = execWriter $ do
  tell "#beginning of mesh obj file \ng\n"
  V.mapM_ (tellV "v") p
  V.mapM_ (tellV "vn") n
  V.mapM_ (tellV "vt") tc
  let
    st a = showText (a+1)
    sc a = st a <> "/" <> st a <> "/" <> st a
  V.mapM_ (\(a1,a2,a3) -> tell $ "f " <> sc a1 <> " " <> sc a2 <> " " <> sc a3 <> "\n") $ i


shuffleIndex :: Face -> Face
shuffleIndex (x,y,z) = (x,z,y)

transformPotatoMeshM44 :: (AnimalFloat a) => M44 a -> PotatoMesh a -> PotatoMesh a
transformPotatoMeshM44 t (PotatoMesh p n tc i) = r where
  -- transform the normals (drop translation and invert scale)
  t' = conv_M44_M33_droptrans t
  nt n' = signorm $ (inv33 $ t') !* n'
  -- shuffle tri indices to maintain correct orientation
  it = if det33 t' > 0 then id else shuffleIndex
  r = PotatoMesh
    (V.map (mul_M44_V3 t) p)
    (V.map nt n)
    tc
    (V.map it i)

-- old LocalMesh stuff, you can delete this

-- TODO maybe get rid of this and use CMesh only
data LocalMesh a = LocalMesh ([V3 a], [Face]) deriving (Show, Generic, NFData)


-- | semigroup instance offsets triangle indices appropriately
instance Semigroup (LocalMesh a) where
  (<>) (LocalMesh (m1,i1)) (LocalMesh (m2, i2)) = LocalMesh (m1++m2, i1 ++ map (map3Tuple (+fromIntegral (length m1))) i2)

instance Monoid (LocalMesh a) where
  mempty = LocalMesh ([],[])
  mappend = (<>)


meshToObj :: (Show a) => LocalMesh a -> T.Text
meshToObj (LocalMesh m) = execWriter $ do
  tell "#beginning of mesh obj file \ng\n"
  mapM_ (tellV "v") $ fst m
  mapM_ (\(a1,a2,a3) -> tell $ "f " <> showText (a1+1) <> " " <> showText (a2+1) <> " " <> showText (a3+1) <> "\n") . snd $ m

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
 verts' = V.unfoldr uncons verts
 faces' = V.unfoldr uncons faces


-- prob can delete this
data MutableCMesh s a = MutableCMesh {
 mcm_vertices :: V.MVector s (V3 a)
 , mcm_faces  :: V.MVector s Face
}
freezeMutableCMesh :: (V.Storable a, PrimMonad m) => MutableCMesh (PrimState m) a -> m (CMesh a)
freezeMutableCMesh (MutableCMesh v f) = CMesh <$> V.freeze v <*> V.freeze f
thawCMesh :: (V.Storable a, PrimMonad m) => CMesh a -> m (MutableCMesh (PrimState m) a)
thawCMesh (CMesh v f) = MutableCMesh <$> V.thaw v <*> V.thaw f
