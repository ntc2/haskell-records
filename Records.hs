{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
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

-- Lens composition.
(#.#) :: Lens b c -> Lens a b -> Lens a c
l1 #.# l2 = Lens (get l1 . get l2) (mod l2 . mod l1)

-- Lenses form a category.
instance Category Lens where
  id  = Lens id id
  (.) = (#.#)

-- Has class: class per label
-- ==========================
--
-- Could use type functions instead ... some people don't like
-- functional dependencies.  But I think fundeps look better here.
--
-- Compare
--
--   _x :: Has_x r t => Lens r t
--
-- with
--
--   _x :: Has_x r   => Lens r (HasT_x r)
--
-- where 'HasT_x' is the type function that computes the type of the
-- 'x' field in 'r'.
--
-- All the classes look the same ... could maybe parameterize by a
-- label, maybe using '-XDataKinds'.
class Has_x r t | r -> t where _x :: Lens r t
class Has_y r t | r -> t where _y :: Lens r t
class Has_z r t | r -> t where _z :: Lens r t

-- For tuples.
class Has_1 r t | r -> t where _1 :: Lens r t
class Has_2 r t | r -> t where _2 :: Lens r t
class Has_3 r t | r -> t where _3 :: Lens r t

-- Record implementation
-- =====================
--
-- Fields should be lexicographically sorted, so that a record type is
-- uniquely determined by the labels and types, independent of the
-- order the labels are given.

-- Tuple in a newtype
-- ------------------
--
-- One 'newtype T_<l1>_..._<ln>' for each record '{l1:t1,...,ln:tn}'
-- in the program.
newtype T_x_y_z tx ty tz = T_x_y_z (tx, ty, tz)
-- A lens into the wrapped tuple.  This facilitates concise 'Has_*'
-- instances for the labels.
t_x_y_z :: Lens (T_x_y_z tx ty tz) (tx, ty, tz)
t_x_y_z = Lens get mod where
  get   (T_x_y_z t) = t
  mod f (T_x_y_z t) = T_x_y_z $ f t

-- Records with one field are special.  The newtype here is the same:
newtype T_x tx = T_x tx
t_x :: Lens (T_x tx) tx
t_x = Lens get mod where
  get   (T_x t) = t
  mod f (T_x t) = T_x $ f t
-- , but the has-instance is degenerate, since the "tuple lens" 't_x'
-- is already the 'Has_x' instance:
instance Has_x (T_x tx) tx where
  _x = _1 . t_x where _1 = id


-- 'Has_*' instances
-- -----------------
instance Has_x (T_x_y_z tx ty tz) tx where
  _x = _1 . t_x_y_z
instance Has_y (T_x_y_z tx ty tz) ty where
  _y = _2 . t_x_y_z
instance Has_z (T_x_y_z tx ty tz) tz where
  _z = _3 . t_x_y_z
-- We use 't_x_y_z', the lens into the underlying tuple, but we could
-- also write the instances directly, e.g.:
{-
instance Has_x (T_x_y_z tx ty tz) tx where
  _x = Lens get' mod' where
    get'   (T_x_y_z t) = get _1 t
    mod' f (T_x_y_z t) = T_x_y_z $ mod _1 f t
-}

-- Tuple 'Has_*' instances
-- =======================

-- 1-tuples
-- --------
--
-- Haskell doesn't have 1-tuples and an identity instance would
-- conflict with all other instances:
{-
instance Has_1 t1 t1 where
  _1 = id
-}

-- 2-tuples
-- --------
instance Has_1 (t1,t2) t1 where
  _1 = Lens get mod where
    get   (x1,x2) = x1 -- fst
    mod f (x1,x2) = (f x1,x2)
instance Has_2 (t1,t2) t2 where
  _2 = Lens get mod where
    get   (x1,x2) = x2 -- snd
    mod f (x1,x2) = (x1,f x2)

-- 3-tuples
-- --------
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

-- 4-tuples, 5-tuples, ...
