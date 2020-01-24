{-|
Module      : Quaternion
Description : helper methods building on top of Linear.Quaternion
Copyright   : (c) Peter Lu, 2018
License     : GPL-3
Maintainer  : chippermonky@email.com
Stability   : experimental

-}


module AnimalClub.Skellygen.Math.Quaternion
    ( identity
    , inverse
    , toM33
    , toM44
    , fromTo
    , fromEulerXYZ
        --lookAt, -- BUGS
    , lookAtDefaultUp -- BUGS
    ) where

import           Lens.Micro.Platform (set)
import           Linear.Conjugate
import           Linear.Epsilon
import qualified Linear.Matrix       as M
import           Linear.Metric
import           Linear.Quaternion
import           Linear.V3
import           Linear.V4

-- |
inverse :: (Conjugate a, RealFloat a) => Quaternion a -> Quaternion a
inverse = conjugate

--invert (Quaternion w (V3 x y z)) = Quaternion wpart v3part where
--	wpart = invns * w'
--	v3part = invns *^ (V3 x' y' z')
--	invns = 1.0 / (v `dot` v)
--	v@(V4 w' x' y' z') = V4 w (-x) (-y) (-z)

identity :: (Num a) => Quaternion a
identity = Quaternion 1 (V3 0 0 0)

-- TODO Test
toM33 :: (RealFloat a, Conjugate a) => Quaternion a -> M.M33 a
toM33 q = V3
  (q `rotate` V3 1 0 0)
  (q `rotate` V3 0 1 0)
  (q `rotate` V3 0 0 1)


toM44 :: (RealFloat a, Conjugate a) => Quaternion a -> M.M44 a
toM44 q = set (_w . _w) 1 (M.m33_to_m44 $ toM33 q)

_orthogonal :: (Num a, Ord a) => V3 a -> V3 a
_orthogonal v@(V3 x y z) = cross v other
  where
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
    q2 = identity -- TODO finish this function is a pain...
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
                 then identity
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
    (axisAngle (V3 0 0 1) z) * (axisAngle (V3 0 1 0) y) *
    (axisAngle (V3 1 0 0) x)
