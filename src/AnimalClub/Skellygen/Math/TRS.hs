{-|
Module      : TRS
Description : types represented affine transformations in R3
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

-}

{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}


module AnimalClub.Skellygen.Math.TRS
  ( Translation
  , Rotation
  , Scale
  , TRS(..)
  , trans
  , rot
  , scale
  , identity
  , axisX, axisY, axisZ
  , up
  , makeScale

  , toM44
  , mul_M44_V3

  , mul_TRS_V3
  , mul_TRS_V4

  , potatoMul

  ) where

import qualified AnimalClub.Skellygen.Math.Quaternion as Q

import           Control.DeepSeq
import           GHC.Generics                         (Generic)
import           Lens.Micro.Platform
import           Linear.Conjugate
import qualified Linear.Matrix                        as M
import           Linear.Quaternion
import           Linear.V3
import           Linear.V4
import           Linear.Vector

-- TODO you can probably get rid of these
type Translation a = V3 a
type Rotation a = Quaternion a
-- TODO change this back to a V3
type Scale a = M.M33 a

-- | matrix::M44 = T * R * S
-- represents affine transformation in R3
data TRS a = TRS
  {
  -- TODO rename to _translation or _pos
  _trans   :: Translation a
  -- TODO rename to _rotation
  , _rot   :: Rotation a
  , _scale :: Scale a -- ^ in actuality, a scale + shear matirx
  } deriving (Show, Generic, NFData)

makeLenses ''TRS

--data TRShS a = TRShS (V3 a) (Quaternion a) (M.M33 a) (V3 a)
--data LocalTRS a = LocalTRS (V3 a) (Quaternion a) (V3 a)

axisX :: (Num a) => V3 a
axisX = V3 1 0 0

axisY :: (Num a) => V3 a
axisY = V3 0 1 0

axisZ :: (Num a) => V3 a
axisZ = V3 0 0 1

up :: (RealFloat a) => TRS a -> V3 a
up trs = mul_TRS_V3 trs axisY

identity :: (Num a) => TRS a
identity = TRS (V3 0 0 0) Q.identity M.identity

makeScale :: (Num a) => V3 a -> M.M33 a
makeScale (V3 x y z) = V3 (V3 x 0 0) (V3 0 y 0) (V3 0 0 z)



m33_to_homogenous_m44 :: (Num a) => M.M33 a -> M.M44 a
m33_to_homogenous_m44 (V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
    V4  (V4 a b c 0)
        (V4 d e f 0)
        (V4 g h i 0)
        (V4 0 0 0 1)

fromTranslation :: (RealFloat a) => Translation a -> M.M44 a
fromTranslation (V3 x y z) =
  V4 (V4 1 0 0 x) (V4 0 1 0 y) (V4 0 0 1 z) (V4 0 0 0 1)

fromRotation :: (Num a) => Rotation a -> M.M33 a
fromRotation = M.fromQuaternion

fromTRS :: (RealFloat a) => TRS a -> M.M44 a
fromTRS (TRS t r s) =
    --m33_to_homogenous_m44 (fromScale s) M.!*! m33_to_homogenous_m44 (fromRotation r) M.!*! fromTranslation t
    fromTranslation t M.!*! m33_to_homogenous_m44 (fromRotation r M.!*! s)
    --M.mkTransformationMat (fromRotation r M.!*! s) t

mul_M44_V3 :: (RealFloat a) => M.M44 a -> V3 a -> V3 a
mul_M44_V3 m v =  normalizePoint $ m M.!* (point v)


toM44 :: (RealFloat a) => TRS a -> M.M44 a
toM44 = fromTRS


mul_TRS_V3 :: (RealFloat a) => TRS a -> V3 a -> V3 a
mul_TRS_V3 trs (V3 x y z) = V3 x' y' z' where V4 x' y' z' _ = mul_TRS_V4 trs (V4 x y z 1)
-- TODO test this by testing both implementations have the same result
--mul_TRS_V3 (TRS pt pr ps) ct = pt ^+^ (pr `rotate` (ps M.!* ct))

mul_TRS_V4 :: (RealFloat a) => TRS a -> V4 a -> V4 a
mul_TRS_V4 trs v = fromTRS trs M.!* v


--_componentDiv :: (RealFloat a) => V3 a -> V3 a -> V3 a
--_componentDiv (V3 ax ay az) (V3 bx by bz) = V3 (ax / bx) (ay / by) (az / bz)

--_componentMul :: (RealFloat a) => V3 a -> V3 a -> V3 a
--_componentMul (V3 ax ay az) (V3 bx by bz) = V3 (ax * bx) (ay * by) (az * bz)

-- |
-- this is probably not very correct
potatoMul :: (Conjugate a, RealFloat a) => TRS a -> TRS a -> TRS a
potatoMul (TRS pt pr ps) (TRS ct cr cs) =
  TRS
    (pt ^+^ (pr `rotate` (ps M.!* ct)))
    (pr * cr)
    (fromRotation (Q.inverse cr) M.!*! ps M.!*! fromRotation cr M.!*! cs)
