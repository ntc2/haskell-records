{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , KindSignatures
  , DataKinds
  , GADTs
  , TypeFamilies
  , ConstraintKinds

  #-}
{-

  -- For HMap class
  , MultiParamTypeClasses
  , FlexibleInstances
  , FunctionalDependencies
  , UndecidableInstances

  , StandaloneDeriving
-}
module Records where

import GHC.Prim (Constraint)



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

-- 'Lens' composition.
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

-- ... or more type safe ...
data LabelKind
 = LT1 -- ^ The label type (LT) of the label "1"
 | LT2
 | LT3
 | LTx
 | LTy
 | LTz

data Label :: LabelKind -> * where
  L1 :: Label LT1
  L2 :: Label LT2
  L3 :: Label LT3
  Lx :: Label LTx
  Ly :: Label LTy
  Lz :: Label LTz

class HasK l r t | l r -> t where
  (##) :: Label l -> Lens r t

-- Labels.  Maybe -XDataKinds could be used to make 'Has' only take
-- label types?
data L_x = L_x
data L_y = L_y
data L_z = L_z



-- Label polymorphism
-- ------------------
--
-- This is an abstraction level above row polymorphism: functions
-- which are polymorphic in the labels they act on, in addition to the
-- rows they act on.

-- (I attempted to use the prefix tick notation to make the lifted
-- clear)

-- Type level lists
data List a = Nil | Cons a (List a)
-- Heterogenous lists indexed by type lists
data HList :: (List *) -> * where
  HNil  :: HList 'Nil
  HCons :: a -> HList as -> HList ('Cons a as)

-- Constraint family.
--
-- The 'List *' here should be ''List *', but that's a syntax error :P
type family   HasAll (ls :: List *) (r :: *) (t :: *) :: Constraint
type instance HasAll 'Nil         r t = ()
type instance HasAll ('Cons l ls) r t = (Has l r t, HasAll ls r t)

-- Use constraint family to qualify type.
--
-- I guess this is a special case of a 'labelFoldr' ...
labelMap :: (HasAll ls r t) => HList ls -> (t -> t) -> (r -> r)
labelMap HNil         _ r = r
labelMap (HCons l ls) f r = mod ((#) l) f (labelMap ls f r)

xyzLabels :: HList ('Cons L_x ('Cons L_y ('Cons L_z 'Nil)))
xyzLabels = HCons L_x (HCons L_y (HCons L_z HNil))

egLabelMap = labelMap xyzLabels (^2) (R_x_y_z 1 2 3)
-- > R_x_y_z 1 4 9


-- Avoid proliferation of label related classes and types
-- ------------------------------------------------------

-- Characters



data Atom = C0 | C1 | C2 | C3
          | Ca | Cb | Cc | Cd
          -- etc
data Word :: List Atom -> * where
  WNil :: Word Nil
  -- Conses
  W0   :: Word cs -> Word (Cons C0 cs)
  W1   :: Word cs -> Word (Cons C1 cs)
  W2   :: Word cs -> Word (Cons C2 cs)
  W3   :: Word cs -> Word (Cons C3 cs)
  Wa   :: Word cs -> Word (Cons Ca cs)
  Wb   :: Word cs -> Word (Cons Cb cs)
  Wc   :: Word cs -> Word (Cons Cc cs)
  Wd   :: Word cs -> Word (Cons Cd cs)
  -- etc

class HasWord (ls :: List Atom) r t where
  lensWord :: Word ls -> Lens r t

-- Some labels
abcLabel :: Word 

-- Alternatively, we can avoid the proliferation of 'Word'
-- constructors by 

-- Characters
data C'0 = C'0
data C'1 = C'1
data C'2 = C'2
data C'3 = C'3
data C'a = C'a
data C'b = C'b
data C'c = C'c
data C'd = C'd

type Word' = HList

class HasWord' ls r t where
  lensWord' :: Word' ls -> Lens r t



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
data R_x_y_z tx ty tz = R_x_y_z tx ty tz deriving Show
-- Or
newtype T_x_y_z tx ty tz = T_x_y_z (tx, ty, tz)
-- A lens for the wrapped tuple.
t_x_y_z :: Lens (T_x_y_z tx ty tz) (tx, ty, tz)
t_x_y_z = Lens get mod where
  get   (T_x_y_z t) = t
  mod f (T_x_y_z t) = T_x_y_z $ f t

instance HasK LTx (R_x_y_z tx ty tz) tx where
  (##) Lx = Lens get mod where
    get   (R_x_y_z x _ _) = x
    mod f (R_x_y_z x y z) = R_x_y_z (f x) y z
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
instance HasK LTx (T_x_y_z tx ty tz) tx where
  (##) Lx = _1 . t_x_y_z

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