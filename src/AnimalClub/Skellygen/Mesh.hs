{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module AnimalClub.Skellygen.Mesh (
    Mesh(..),
    emptyMesh,
    meshToObj,
    transformMesh,
    transformMeshM44

) where

import qualified AnimalClub.Skellygen.TRS    as TRS

import           Control.DeepSeq
import           Control.Monad.Writer.Lazy   (Writer, execWriter, tell)
import           GHC.Generics                (Generic)

import           Data.Monoid                 (Monoid, mappend)
import           Data.Semigroup              (Semigroup, (<>))
import qualified Data.Vector.Storable        as S

import           AnimalClub.Skellygen.Linear
import           AnimalClub.Skellygen.TRS



data Mesh a = Mesh ([V3 a], [(Int,Int,Int)]) deriving (Generic, NFData)

emptyMesh :: Mesh a
emptyMesh = Mesh ([],[])

map3Tuple :: (a->b) -> (a,a,a) -> (b,b,b)
map3Tuple f (a1,a2,a3) = (f a1, f a2, f a3)

-- | semigroup instance offsets triangle indices appropriately
instance Semigroup (Mesh a) where
    (<>) (Mesh (m1,i1)) (Mesh (m2, i2)) = Mesh (m1++m2, i1 ++ map (map3Tuple (+length m1)) i2)

instance Monoid (Mesh a) where
    mempty = Mesh ([],[])
    mappend = (<>)

tellV3 :: (Show a) =>  V3 a -> Writer String ()
tellV3 v = do
    tell "v "
    mapM_ (\tv -> tell $ show tv ++ " ") $ v
    tell "\n"

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"

-- TODO change this to Data.Text
meshToObj :: (Show a) => Mesh a -> String
meshToObj (Mesh m) = execWriter $ do
    tell "#beginning of mesh obj file \ng\n"
    mapM_ tellV3 $ fst m
    mapM_ (\(a1,a2,a3) -> tell $ "f " ++ show (a1+1) ++ " " ++ show (a2+1) ++ " " ++ show (a3+1) ++ "\n") . snd $ m

transformMesh :: (AnimalFloat a) => TRS a -> Mesh a -> Mesh a
transformMesh trs (Mesh (verts, inds)) =  Mesh (map mapfn verts, inds) where
    mapfn = mul_TRS_V3 trs

transformMeshM44 :: (AnimalFloat a) => M44 a -> Mesh a -> Mesh a
transformMeshM44 trs (Mesh (verts, inds)) =  Mesh (map mapfn verts, inds) where
    mapfn = mul_M44_V3 trs
