> {-# LANGUAGE MultiParamTypeClasses
>   , FunctionalDependencies
>   , FlexibleInstances
>   , KindSignatures
>   , DataKinds
>   , GADTs
>   , TypeFamilies
>   , ConstraintKinds
>   , FlexibleContexts
>   , UndecidableInstances
>   #-}
> import GHC.Prim (Constraint)

Avoid proliferation of 'Has_*' classes and label types (with 'Has *'
class) by defining singleton types for label strings.  This encoding
is a special case of a type-level lists and term-level het lists
encoding.



Type level label strings:

> data TLabel
>   = T         -- ^ Nil
>   | T0 TLabel -- ^ Cons 0
>   | T1 TLabel -- ^ Cons 1

and a term level singleton type of indexed labels:

> data Label :: TLabel -> * where
>   L  :: Label T                 -- ^ Nil
>   L0 :: Label l -> Label (T0 l) -- ^ Cons 0
>   L1 :: Label l -> Label (T1 l) -- ^ Cons 1

Has class:

> class Has (l :: TLabel) r t | l r -> t where
>   (#) :: Label l -> Lens r t

Example:

> l0110 :: Label (T0 (T1 (T1 (T0 T))))
> l0110 =        (L0 (L1 (L1 (L0 L))))

Now you imagine e.g. that

    #0110

compiles to

    (#) l0110



Can we avoid the proliferation of record types as well?  Sure, in the
same way a for finite tuple encodings:

XXX: do i need to ensure that 'ls' and 'ts' have the same length?

> type family   HasAll (ls :: List TLabel) (ts :: List *) r :: Constraint
> type instance HasAll 'Nil         'Nil         r = ()
> type instance HasAll ('Cons l ls) ('Cons t ts) r = 
>   (Has l r t, HasAll ls ts r)

> data Rec :: List TLabel -> List * -> * where
>   RNil  :: Rec Nil Nil
>   RCons :: Label     l     ->    t
>         -> Rec         ls          ts
>         -> Rec (Cons l ls) (Cons t ts)

> instance (l ~ l') =>
>   Has l (Rec (Cons l' ls) (Cons t ts)) t where
>     (#) _ = Lens get upd where
>       get :: (Rec (Cons l' ls) (Cons t ts)) -> t
>       get   (RCons _ t _) = t
>       upd :: (t -> t) -> (Rec (Cons l' ls) (Cons t ts))
>                       -> (Rec (Cons l' ls) (Cons t ts))
>       upd f (RCons l t r) = RCons l (f t) r

> instance (Has l (Rec ls ts) t) =>
>   Has l (Rec (Cons l' ls) (Cons t' ts)) t where
>     (#) l = Lens get' upd' where
>       get' :: (Rec (Cons l' ls) (Cons t' ts)) -> t
>       get'   (RCons _ _ r) = get ((#) l) r
>       upd' :: (t -> t) -> (Rec (Cons l' ls) (Cons t' ts))
>                        -> (Rec (Cons l' ls) (Cons t' ts))
>       upd' f (RCons l t r) = RCons l t (upd ((#) l) f r)
> 


Boilerplate:

XXX: I really need to put these defs in a separate file so don't have
to type them over and over ...

> data Lens r t = Lens { get :: r -> t
>                      , upd :: (t -> t) -> (r -> r)
>                      }

> data List a = Nil | Cons a (List a)