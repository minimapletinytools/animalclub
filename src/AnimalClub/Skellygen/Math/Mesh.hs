{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module AnimalClub.Skellygen.Math.Mesh (
    Mesh(..),
    emptyMesh,
    meshToObj,
    transformMesh
) where

import qualified AnimalClub.Skellygen.Math.TRS as TRS

import           Control.DeepSeq
import           Control.Monad.Writer.Lazy     (Writer, execWriter, tell)
import           GHC.Generics                  (Generic)

import           Data.Monoid                   (Monoid, mappend)
import           Data.Semigroup                (Semigroup, (<>))

import           Linear.V3



data Mesh = Mesh ([V3 Float], [Int]) deriving (Generic, NFData)


emptyMesh :: Mesh
emptyMesh = Mesh ([],[])

instance Semigroup Mesh where
    (<>) (Mesh (m1,i1)) (Mesh (m2, i2)) = Mesh (m1++m2, i1 ++ map (\x->x+length m1) i2)

instance Monoid Mesh where
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
meshToObj :: Mesh -> String
meshToObj (Mesh m) = execWriter $ do
    tell "#beginning of mesh obj file \ng\n"
    mapM_ tellV3 $ fst m
    mapM_ (\x -> tell $ "f " ++ foldr (\y acc -> acc ++ " " ++ show (y+1)) "" x ++ "\n") . group 3 . snd $ m

-- TODO rewrite this using M44
transformMesh :: TRS.TRS Float -> Mesh -> Mesh
transformMesh trs (Mesh (verts, inds)) =  Mesh (map mapfn verts, inds) where
    mapfn = TRS.transformV3 trs
