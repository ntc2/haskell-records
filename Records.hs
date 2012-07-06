{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
--  , KindSignatures
--  , DataKinds
  #-}
module Records where

import Prelude hiding (mod, (.), id)
import Control.Category

-- Lens data type
-- ==============

data Lens a b = Lens (a -> b)               -- ^ get
                     ((b -> b) -> (a -> a)) -- ^ mod

get :: Lens a b -> a -> b
get (Lens g _) = g

mod :: Lens a b -> (b -> b) -> (a -> a)
mod (Lens _ m) = m

set :: Lens a b -> b -> a -> a
set l x = mod l (const x)

-- 'Lens' coposition.
(#.#) :: Lens b c -> Lens a b -> Lens a c
l1 #.# l2 = Lens (get l1 . get l2) (mod l2 . mod l1)
-- Can use 'Control.Category' instead.
instance Category Lens where
  id = Lens id id
  (.) = (#.#)

-- Has classes
-- ===========
--
-- Could use type functions instead ... some people don't like
-- functional dependencies.  I think fundeps look better here.
-- Compare e.g.
--
--   Has_x l r t => r -> t
--
-- with
--
--   Has_x l r => r -> HasT_x r
--
-- where 'HasT_x' is the type function that computes the type of the
-- 'x' field in 'r'.

-- Has: One class + label types
-- ----------------------------
class Has l r t | l r -> t where
  (#) :: l -> Lens r t

-- Labels.  Maybe -XDataKinds could be used to make 'Has' only take
-- label types?
data L_x = L_x
data L_y = L_y
data L_z = L_z

-- Has: Class per label
-- --------------------
class Has_x r t | r -> t where
  _x :: Lens r t
class Has_y r t | r -> t where
  _y :: Lens r t
class Has_z r t | r -> t where
  _z :: Lens r t

class Has_1 r t | r -> t where
  _1 :: Lens r t
class Has_2 r t | r -> t where
  _2 :: Lens r t
class Has_3 r t | r -> t where
  _3 :: Lens r t

-- Record implementation: tagged tuple
-- -----------------------------------
--
-- Fields should be lex sorted, so that record type is uniquely
-- determined by labels, independent of order.

-- Record with fields 'x', 'y', and 'z'
data R_x_y_z tx ty tz = R_x_y_z tx ty tz
-- Or
newtype T_x_y_z tx ty tz = T_x_y_z (tx, ty, tz)
-- A lens for the wrapped tuple.
t_x_y_z :: Lens (T_x_y_z tx ty tz) (tx, ty, tz)
t_x_y_z = Lens get mod where
  get   (T_x_y_z t) = t
  mod f (T_x_y_z t) = T_x_y_z $ f t

instance Has L_x (R_x_y_z tx ty tz) tx where
  (#) L_x = Lens get mod where
    get   (R_x_y_z x _ _) = x
    mod f (R_x_y_z x y z) = R_x_y_z (f x) y z
instance Has_x (R_x_y_z tx ty tz) tx where
  _x = Lens get mod where
    get   (R_x_y_z x _ _) = x
    mod f (R_x_y_z x y z) = R_x_y_z (f x) y z

instance Has L_y (R_x_y_z tx ty tz) ty where
  (#) L_y = Lens get mod where
    get   (R_x_y_z _ y _) = y
    mod f (R_x_y_z x y z) = R_x_y_z x (f y) z
instance Has_y (R_x_y_z tx ty tz) ty where
  _y = Lens get mod where
    get   (R_x_y_z _ y _) = y
    mod f (R_x_y_z x y z) = R_x_y_z x (f y) z

instance Has L_z (R_x_y_z tx ty tz) tz where
  (#) L_z = Lens get mod where
    get   (R_x_y_z _ _ z) = z
    mod f (R_x_y_z x y z) = R_x_y_z x y (f z)
instance Has_z (R_x_y_z tx ty tz) tz where
  _z = Lens get mod where
    get   (R_x_y_z _ _ z) = z
    mod f (R_x_y_z x y z) = R_x_y_z x y (f z)

-- Tuple versions

-- Written out by hand:
{-
instance Has_x (T_x_y_z tx ty tz) tx where
  _x = Lens get' mod' where
    get'   (T_x_y_z t) = get _1 t
    mod' f (T_x_y_z t) = T_x_y_z $ mod _1 f t
instance Has_y (T_x_y_z tx ty tz) ty where
  _y = Lens get' mod' where
    get'   (T_x_y_z t) = get _2 t
    mod' f (T_x_y_z t) = T_x_y_z $ mod _2 f t
instance Has_z (T_x_y_z tx ty tz) tz where
  _z = Lens get' mod' where
    get'   (T_x_y_z t) = get _3 t
    mod' f (T_x_y_z t) = T_x_y_z $ mod _3 f t
-}
-- Or using the tuple lens:
instance Has_x (T_x_y_z tx ty tz) tx where
  _x = _1 . t_x_y_z
instance Has_y (T_x_y_z tx ty tz) ty where
  _y = _2 . t_x_y_z
instance Has_z (T_x_y_z tx ty tz) tz where
  _z = _3 . t_x_y_z

-- Tuple 'Has_*' instances
-- -----------------------

-- Can't make an identity instance because it conflicts with all other
-- instances.  I guess we're seeing that not having a type of 1-tuples
-- makes us lose some uniformity.
{-
instance Has_1 t1 t1 where
  _1 = id
-}
-- However, we could create a newtype to achieve this:
newtype Wrap t = Wrap t
instance Has_1 (Wrap t) t where
  _1 = Lens get mod where
    get   (Wrap x1) = x1
    mod f (Wrap x1) = Wrap $ f x1
instance Has_1 (t1,t2) t1 where
  _1 = Lens get mod where
    get   (x1,x2)  = x1
    mod f (x1,x2) = (f x1,x2)
instance Has_2 (t1,t2) t2 where
  _2 = Lens get mod where
    get   (x1,x2) = x2
    mod f (x1,x2) = (x1,f x2)

instance Has_1 (t1,t2,t3) t1 where
  _1 = Lens get mod where
    get   (x1,x2,x3) = x1
    mod f (x1,x2,x3) = (f x1,x2,x3)
instance Has_2 (t1,t2,t3) t2 where
  _2 = Lens get mod where
    get   (x1,x2,x3) = x2
    mod f (x1,x2,x3) = (x1,f x2,x3)
instance Has_3 (t1,t2,t3) t3 where
  _3 = Lens get mod where
    get   (x1,x2,x3) = x3
    mod f (x1,x2,x3) = (x1,x2,f x3)

{-
-- Not sure this makes any sense ...


-- Alternative: make get, set, and mod part of the type class, and
-- then give default implementations for mod and set in terms of the
-- other.
class Has_lC r t | r -> t where
  _lC :: (LensC r t a) => a

class LensC r t a | r t -> a where
  getC :: r -> t
  setC :: t -> (r -> r)
  modC :: (t -> t) -> (r -> r)
  modC f x = setC (f $ getC x) x
  setC c x = modC (const c) x

-- Now composition is defined by an instance
instance (LensC b c, LensC a b) => LensC a c where
  getC = getC . getC
  modC = modC . modC
-}