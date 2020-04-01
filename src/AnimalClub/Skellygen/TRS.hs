{-|
Module      : TRS
Description : types represented affine transformations in R3
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}


module AnimalClub.Skellygen.TRS
(
  AnimalFloat

  -- TRS components
  , Translation
  , Rotation
  , Scale

  -- TRS
  , TRS(..)
  , trans, rot, scale
  , identityTRS

  -- TRS operations
  , conv_TRS_M44
  , mul_TRS_V3
  , mul_TRS_V4
  , lossyScaleTRS

  -- matrix helper
  , mul_M44_V3
  , conv_M44_M33_droptrans

  -- scale operations
  , identityScale
  , conv_Scale_M33
  , mul_Scale_M44

  -- quaternion/rotation operations
  , identityRotation
  , rotationInverse
  , conv_Rotation_M33
  , conv_Rotation_M44
  , fromTo
  , fromEulerXYZ
  --lookAt, -- BUGS
  , lookAtDefaultUp -- BUGS
) where

import           Relude

import           AnimalClub.Skellygen.Linear
import           Control.DeepSeq
import           GHC.Generics                (Generic)
import           Lens.Micro.Platform
import           Linear.Conjugate

-- | constraint kind needed for math operations to work properly (just use Float or Double in practice)
type AnimalFloat a = (Conjugate a, RealFloat a, Epsilon a, Show a)

-- TODO you can probably get rid of these
type Translation a = V3 a
type Rotation a = Quaternion a
-- TODO change this back to a V3
type Scale a = V3 a

-- | matrix::M44 = T * R * S
-- represents affine transformation in R3
data TRS a = TRS {
    -- TODO rename to _translation or _pos
    _trans   :: Translation a
    -- TODO rename to _rotation
    , _rot   :: Rotation a
    , _scale :: Scale a
  } deriving (Show, Generic, NFData)

makeLenses ''TRS

--data TRShS a = TRShS (V3 a) (Quaternion a) (M33 a) (V3 a)
--data LocalTRS a = LocalTRS (V3 a) (Quaternion a) (V3 a)


{-
axisX :: (Num a) => V3 a
axisX = V3 1 0 0

axisY :: (Num a) => V3 a
axisY = V3 0 1 0

axisZ :: (Num a) => V3 a
axisZ = V3 0 0 1

up :: (RealFloat a) => TRS a -> V3 a
up trs = mul_TRS_V3 trs axisY
-}

identityTRS :: (Num a) => TRS a
identityTRS = TRS (V3 0 0 0) identityRotation identityScale

m33_to_homogenous_m44 :: (Num a) => M33 a -> M44 a
m33_to_homogenous_m44 (V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
  V4  (V4 a b c 0)
    (V4 d e f 0)
    (V4 g h i 0)
    (V4 0 0 0 1)

fromTranslation :: (RealFloat a) => Translation a -> M44 a
fromTranslation (V3 x y z) =
 V4 (V4 1 0 0 x) (V4 0 1 0 y) (V4 0 0 1 z) (V4 0 0 0 1)

mul_M44_V3 :: (RealFloat a) => M44 a -> V3 a -> V3 a
mul_M44_V3 m v =  normalizePoint $ m !* (point v)

conv_TRS_M44 :: (RealFloat a) => TRS a -> M44 a
conv_TRS_M44 (TRS t r s) = fromTranslation t !*! m33_to_homogenous_m44 (conv_Rotation_M33 r !*! conv_Scale_M33 s)

-- | convert a homegenous M44 to M33 dropping all translation components
conv_M44_M33_droptrans :: M44 a -> M33 a
conv_M44_M33_droptrans = view _m33

mul_TRS_V3 :: (RealFloat a) => TRS a -> V3 a -> V3 a
mul_TRS_V3 trs (V3 x y z) = V3 x' y' z' where V4 x' y' z' _ = mul_TRS_V4 trs (V4 x y z 1)

mul_TRS_V4 :: (RealFloat a) => TRS a -> V4 a -> V4 a
mul_TRS_V4 trs v = conv_TRS_M44 trs !* v

mul_Scale_M44 :: (RealFloat a) => Scale a -> M44 a -> M44 a
mul_Scale_M44 (V3 x y z) (V4 c1 c2 c3 c4) = V4 (x*^c1) (y*^c2) (z*^c3) c4


identityScale :: (Num a) => Scale a
identityScale = V3 1 1 1

conv_Scale_M33 :: (Num a) => Scale a -> M33 a
conv_Scale_M33 (V3 x y z) = V3 (V3 x 0 0) (V3 0 y 0) (V3 0 0 z)

-- same as getDiag in Data.Matrix
get_diagonal :: M33 a -> Scale a
get_diagonal (V3 (V3 x _ _) (V3 _ y _) (V3 _ _ z)) = V3 x y z



--_componentDiv :: (RealFloat a) => V3 a -> V3 a -> V3 a
--_componentDiv (V3 ax ay az) (V3 bx by bz) = V3 (ax / bx) (ay / by) (az / bz)

_componentMul :: (RealFloat a) => V3 a -> V3 a -> V3 a
_componentMul (V3 ax ay az) (V3 bx by bz) = V3 (ax * bx) (ay * by) (az * bz)

-- | scales a TRS (interpreting scale in parent space)
-- N.B. this throws out shear components that may result!
lossyScaleTRS :: (AnimalFloat a) => Scale a -> TRS a -> TRS a
lossyScaleTRS ps (TRS ct cr cs) = TRS
  (_componentMul ps ct)
  cr
  (get_diagonal $
    conv_Rotation_M33 (rotationInverse cr)
    !*! conv_Scale_M33 ps
    !*! conv_Rotation_M33 cr
    !*! conv_Scale_M33 cs)

-- |
rotationInverse :: (Conjugate a, RealFloat a) => Quaternion a -> Quaternion a
rotationInverse = conjugate

--invert (Quaternion w (V3 x y z)) = Quaternion wpart v3part where
--	wpart = invns * w'
--	v3part = invns *^ (V3 x' y' z')
--	invns = 1.0 / (v `dot` v)
--	v@(V4 w' x' y' z') = V4 w (-x) (-y) (-z)

identityRotation :: (Num a) => Quaternion a
identityRotation = Quaternion 1 (V3 0 0 0)

-- TODO Test
conv_Rotation_M33 :: (Num a) => Quaternion a -> M33 a
conv_Rotation_M33 = fromQuaternion


conv_Rotation_M44 :: (RealFloat a) => Quaternion a -> M44 a
conv_Rotation_M44 q = set (_w . _w) 1 (m33_to_m44 $ conv_Rotation_M33 q)

-- how is this even compiling?
_orthogonal :: (Num a, Ord a) => V3 a -> V3 a
_orthogonal v@(V3 x y z) = cross v other where
  other =
    if abs x < abs y
      then if abs x < abs z
          then V3 1 0 0
          else V3 0 0 1
      else if abs y < abs z
          then V3 0 1 0
          else V3 0 0 1

--_radiansBetween :: (Metric f) => f a -> f a -> a
--_radiansBetween a b = undefined -- acos $ (a `dot` b) / (norm a * norm b)

-- | assumes neutral look is toward (V3 1 0 0) with up towards (V3 0 1 0) as neutral position
-- TODO finish
-- TODO wtf is tis interface...
lookAt ::
   (RealFloat a, Epsilon a)
  => V3 a -- ^ up vector
  -> V3 a -- ^ fallback up vector
  -> V3 a -- ^ direction to look at
  -> Quaternion a -- ^ output
lookAt _ _ d = q2 * q
    --zup = V3 0 1 0
    --up = if nearZero (d-u) then fu else u
    --q2 = axisAngle d (_radiansBetween d up)
  where
    q2 = identityRotation -- TODO finish this function is a pain...
    q = fromTo (V3 1 0 0) d

-- TODO rename this
lookAtDefaultUp ::
   (RealFloat a, Epsilon a)
  => V3 a -- ^ direction to look at
  -> Quaternion a -- ^ output
lookAtDefaultUp = lookAt (V3 0 1 0) (V3 1 0 0)

-- | from -> to -> rotation
fromTo :: (RealFloat a, Epsilon a) => V3 a -> V3 a -> Quaternion a
fromTo fromv tov =
  if nearZero (u + v)
    then fallback
    else if nearZero (u - v)
        then identityRotation
        else output
  where
    u = normalize fromv
    v = normalize tov
    half = normalize (u + v)
    fallback = Quaternion 0 (normalize $ _orthogonal u)
    output = Quaternion (dot u half) (cross u half)

-- FUTURE make fancy version that is fromEuler :: XYZ -> V3 a -> Quternion a
-- | x y z rotation order
fromEulerXYZ :: (RealFloat a, Epsilon a) => V3 a -> Quaternion a
fromEulerXYZ (V3 x y z) =
  (axisAngle (V3 0 0 1) z)
  * (axisAngle (V3 0 1 0) y)
  * (axisAngle (V3 1 0 0) x)


{-
TRS with scale matrix is just a bad idea, but this is what the math looks like if we were to do it:

-- |
-- this is the correct implementation for combining two TRSs I believe
-- requires scale component to be a 3x3 matrix
-- assuming T R S is closed under multiplication (prove by representing as 4x4 homogenous matrix)
-- w.t.s $$T*R*S = (T1*R1*S1) * (T2*R2*S2)$$
-- since $T*v = TRS*v$ where $$v=[0,0,0,1]$$ is a point at the origin (in R3)
-- and this entirely determines $T$
-- we have $$T = T1 * (R1*S1*T2)$$
-- by examining matrix decomposition of a homogenous M44
-- we get that in general the rotation and scale components as represented as M33 are not affected by the translation component
-- in particular $$m33(R*S*T) = m33(T*R*S)$$ where $$m33$$ is the upper left 3x3 submatrix
-- thus we have $$R*S = R1*S1*R2*S2$$
-- by taking $$R = R1*R2$$
-- we multiply both sides by $$invR = invR2*invR1$$ to get
-- $$S = invR2*S1*R2*S2$$
-- $$S$$ will not be a diagonal matrix in general
potatoMul :: (AnimalFloat a) => TRS a -> TRS a -> TRS a
potatoMul (TRS pt pr ps) (TRS ct cr cs) =
 TRS
   (pt ^+^ (pr `rotate` (ps !* ct)))
   (pr * cr)
   (conv_Rotation_M33 (rotationInverse cr) !*! ps !*! conv_Rotation_M33 cr !*! cs)

prop_potatoMul :: Scale Double -> TRS Double -> Bool
prop_potatoMul trs1 trs2 = pass where
 p = conv_TRS_M44 $ potatoMul trs1 trs2
 p' = conv_TRS_M44 trs1 !*! conv_TRS_M44 trs2
 pass = nearZero (p-p')
-}
