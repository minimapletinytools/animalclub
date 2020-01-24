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

  , transformV3
  , transformV4


  , invTRS
  ) where

import           AnimalClub.Skellygen.Math.Hierarchical
import qualified AnimalClub.Skellygen.Math.Quaternion   as QH

import           Control.DeepSeq
import           GHC.Generics                           (Generic)
import           Lens.Micro.Platform
import           Linear.Conjugate
import qualified Linear.Matrix                          as M
import           Linear.Quaternion
import           Linear.V3
import           Linear.V4
import           Linear.Vector

-- TODO you can probably get rid of these
type Translation a = V3 a
type Rotation a = Quaternion a
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
up trs = transformV3 trs axisY

identity :: (Num a) => TRS a
identity = TRS (V3 0 0 0) QH.identity M.identity

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

toM44 :: (RealFloat a) => TRS a -> M.M44 a
toM44 = fromTRS

mul_M44_V3 :: (RealFloat a) => M.M44 a -> V3 a -> V3 a
mul_M44_V3 m v =  normalizePoint $ m M.!* (point v)


-- TODO rename to mul_TRS_V3
transformV3 :: (RealFloat a) => TRS a -> V3 a -> V3 a
--transformV3 (TRS pt pr ps) ct = pt ^+^ (pr `rotate` (ps M.!* ct))
transformV3 trs (V3 x y z) = V3 x' y' z' where V4 x' y' z' _ = transformV4 trs (V4 x y z 1)

transformV4 :: (RealFloat a) => TRS a -> V4 a -> V4 a
transformV4 trs v = fromTRS trs M.!* v


--_componentDiv :: (RealFloat a) => V3 a -> V3 a -> V3 a
--_componentDiv (V3 ax ay az) (V3 bx by bz) = V3 (ax / bx) (ay / by) (az / bz)

--_componentMul :: (RealFloat a) => V3 a -> V3 a -> V3 a
--_componentMul (V3 ax ay az) (V3 bx by bz) = V3 (ax * bx) (ay * by) (az * bz)

-- |
-- TODO this is wrong :(((( DELETE
-- inherit P C returns P * C, i.e. C in the frame of P
--
-- TODO enable latex formatter 😱
-- proof this is correct:
-- assuming T R S is closed under multiplication (prove by representing as 4x4 homogenous matrix)
-- w.t.s $$T*R*S = (T1*R1*S1) * (T2*R2*S2)$$
-- since $T*v = TRS*v$ where $$v=[0,0,0,1]$$ is a point at the origin (in R3)
-- and this entirely determines $T$
-- we have $$T = T1 * (R1*S1*T2)$$
-- by examining matrix decomposition of a homogenous M44 (i.e. the first 3 entires of the bottom row are 0)
-- we get that in general the rotation and scale components as represented as M33 are not affected by the translation component
-- in particular $$m33(R*S*T) = m33(T*R*S)$$ where $$m33$$ is the upper left 3x3 submatrix
-- thus we have $$R*S = R1*S1*R2*S2$$
-- by taking $$R = R1*R2$$
-- we multiply both sides by $$invR = invR2*invR1$$ to get
-- $$S = invR2*S1*R2*S2$$
-- our proof is complete once we show that $$S$$ has no rotation components in it.
-- questions:
--  (1) can we further decompose $$S = Sh * S'$$ where $$Sh$$ is a shear matrix and $$S'$$ is a diagonal scale matirx?
instance (Conjugate a, RealFloat a) => Hierarchical (TRS a) where
  inherit (TRS pt pr ps) (TRS ct cr cs) =
    TRS
      (pt ^+^ (pr `rotate` (ps M.!* ct)))
      (pr * cr)
      (fromRotation (QH.inverse cr) M.!*! ps M.!*! fromRotation cr M.!*! cs)


-- TODO this is wrong DELETE
-- | inverts TRS (WIP)
-- $$ (T*R*S)^{-1} = S^{-1}*R^{-1}*T^{-1}$$
-- given that $$ m33(S^{-1}*R^{-1}*T^{-1}) = m33(T^{-1}*S^{-1}*R^{-1}) $$
-- $$ R'*S' = S^{-1}*R^{-1} $$
-- take $$ R' = R^{-1} $$
-- then $$ S' = R*S^{-1}*R^{-1} $$
-- must show $$S'$$ has no rotation component in it
-- $$ T'R'S' = T'R^{-1}*R*S^{-1}*R^{-1} = T'S^{-1}*R^{-1} = S^{-1}*R^{-1}*T^{-1} $$
-- thus $$ T' =  S^{-1}*R^{-1}*T^{-1}*R*S $$
-- must show that $$T'$$ is still a translation
invTRS :: (RealFloat a, Conjugate a) => TRS a -> TRS a
invTRS (TRS t r s) = TRS t' r' s' where
  r' = QH.inverse r
  matr = QH.toM33 r
  matr' = QH.toM33 r'
  invs = M.inv33 s
  s' = matr M.!*! invs M.!*! matr'
  t' = invs M.!* (matr' M.!* (-t))
