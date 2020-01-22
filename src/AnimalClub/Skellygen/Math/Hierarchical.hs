module AnimalClub.Skellygen.Math.Hierarchical (
    Hierarchical(..),
    (>*>),
    AbsOrRel(..),
    unAbsOrRel
) where


import           Linear.Quaternion
import           Linear.V3

class Hierarchical e where
    -- | parent -> child
    inherit :: e -> e -> e

infixl 7 >*>
(>*>) :: (Hierarchical e) => e -> e -> e
(>*>) = inherit

-- | indicates whether to treat the contained object as relative or absolute to its parent
data AbsOrRel a = Abs a | Rel a deriving (Functor, Show)

unAbsOrRel :: AbsOrRel a -> a
unAbsOrRel (Abs a) = a
unAbsOrRel (Rel a) = a

instance (Hierarchical a) => Hierarchical (AbsOrRel a) where
    inherit x y = case y of
        Abs a -> Abs a
        Rel a -> Abs $ inherit (unAbsOrRel x) a

instance (Num a) => Hierarchical (V3 a) where
    inherit x y = y + x

instance (RealFloat a) => Hierarchical (Quaternion a) where
    inherit x y = x * y

instance Hierarchical Float where
    inherit x y = x * y
