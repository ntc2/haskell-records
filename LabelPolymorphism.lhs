> {-# LANGUAGE MultiParamTypeClasses
>   , FunctionalDependencies
>   , FlexibleInstances
>   , KindSignatures
>   , DataKinds
>   , GADTs
>   , TypeFamilies
>   , ConstraintKinds
>   #-}
> import Prelude hiding ((.), id)
> import Control.Category
> import GHC.Prim (Constraint)

Label polymorphism
==================

This is an abstraction level above row polymorphism: functions are
polymorphic in the labels they act on, in addition to the rows they
act on.

One `Has` class to rule them all
--------------------------------

> class Has l r t | l r -> t where
>   (#) :: l -> Lens r t

Singleton types for labels
--------------------------

> data L_1 = L_1
> data L_2 = L_2
> data L_3 = L_3
> data L_x = L_x
> data L_y = L_y
> data L_z = L_z

Example: applying a function for all labels in a list
=====================================================

Lists for use at the type level
-------------------------------

> data List a = Nil | Cons a (List a)

Heterogenous lists (indexed by type lists)
------------------------------------------

> data HList :: (List *) -> * where
>   HNil  :: HList Nil
>   HCons :: a -> HList as -> HList (Cons a as)

Constraint family: `r` has all fields in `ls` with same type `t`
----------------------------------------------------------------

> type family   HasAll (ls :: List *) (r :: *) (t :: *) :: Constraint
> type instance HasAll Nil         r t = ()
> type instance HasAll (Cons l ls) r t = (Has l r t, HasAll ls r t)

Apply `f` to every field in `ls`
--------------------------------

> labelMap :: (HasAll ls r t) => HList ls -> (t -> t) -> (r -> r)
> labelMap HNil         _ r = r
> labelMap (HCons l ls) f r = upd ((#) l) f (labelMap ls f r)

Example
-------

> xzLabels :: HList (Cons L_x  (Cons L_z Nil))
> xzLabels =        HCons L_x (HCons L_z HNil)

> egLabelMap = labelMap xzLabels (^2) (T_x_y_z (2, 3, 4))

    ghci> egLabelMap
    T_x_y_z (4, 3, 16)


Record implementation (about the same as before)
================================================

`Has` instances about the same
------------------------------

> instance Has L_x (T_x_y_z tx ty tz) tx where
>   (#) L_x = (#) L_1 . t_x_y_z
> instance Has L_y (T_x_y_z tx ty tz) ty where
>   (#) L_y = (#) L_2 . t_x_y_z
> instance Has L_z (T_x_y_z tx ty tz) tz where
>   (#) L_z = (#) L_3 . t_x_y_z

> instance Has L_1 (t1,t2,t3) t1 where
>   (#) L_1 = Lens get upd where
>     get   (x1,x2,x3) = x1
>     upd f (x1,x2,x3) = (f x1,x2,x3)
> instance Has L_2 (t1,t2,t3) t2 where
>   (#) L_2 = Lens get upd where
>     get   (x1,x2,x3) = x2
>     upd f (x1,x2,x3) = (x1,f x2,x3)
> instance Has L_3 (t1,t2,t3) t3 where
>   (#) L_3 = Lens get upd where
>     get   (x1,x2,x3) = x3
>     upd f (x1,x2,x3) = (x1,x2,f x3)

Same tuple lenses
-----------------

> newtype T_x_y_z tx ty tz = T_x_y_z (tx, ty, tz) deriving Show

> t_x_y_z :: Lens (T_x_y_z tx ty tz) (tx, ty, tz)
> t_x_y_z = Lens get upd where
>   get   (T_x_y_z t) = t
>   upd f (T_x_y_z t) = T_x_y_z $ f t

Same lens data type
-------------------

> data Lens a b = Lens (a -> b)               -- ^ get / view
>                      ((b -> b) -> (a -> a)) -- ^ update

> get :: Lens a b -> a -> b
> get (Lens g _) = g

> upd :: Lens a b -> (b -> b) -> (a -> a)
> upd (Lens _ m) = m

> set :: Lens a b -> b -> a -> a
> set l x = upd l (const x)

> composeLens :: Lens b c -> Lens a b -> Lens a c
> l1 `composeLens` l2 = Lens (get l1 . get l2)
>                            (upd l2 . upd l1)

> idLens :: Lens a a
> idLens = Lens id id

> instance Category Lens where
>   id  = idLens
>   (.) = composeLens
