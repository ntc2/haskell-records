> {-# LANGUAGE MultiParamTypeClasses
>   , FunctionalDependencies
>   , FlexibleInstances
>   , KindSignatures
>   , DataKinds
>   , GADTs
>   , TypeFamilies
>   , ConstraintKinds
>   , StandaloneDeriving
>   -- , InstanceSigs
>   , FlexibleContexts
>   , UndecidableInstances
>   , OverlappingInstances
>   #-}
> import GHC.Prim (Constraint)

Big Picture
===========

*Fixed* and *finite* set of types and classes for constructing *all*
labels, records, and `Has` classes.

<!--
Avoid proliferation of `Has_*` classes and label types (with `Has *`
class) by defining singleton types for label strings.  This encoding
is a special case of a type-level lists and term-level het lists
encoding.
-->

Labels
======

Singleton types
---------------

Type level label strings:

> data TLabel
>   = T         -- ^ Nil
>   | T0 TLabel -- ^ Cons 0
>   | T1 TLabel -- ^ Cons 1
>   deriving Show

and a term level singleton type of indexed labels:

> data Label :: TLabel -> * where
>   L  :: Label T                 -- ^ Nil
>   L0 :: Label l -> Label (T0 l) -- ^ Cons 0
>   L1 :: Label l -> Label (T1 l) -- ^ Cons 1
> deriving instance Show (Label l)

Has class
---------

> class Has (l :: TLabel) r t | l r -> t where
>   (#) :: Label l -> Lens r t

Example
-------

> l0110 :: Label (T0 (T1 (T1 (T0 T))))
> l0110 =        (L0 (L1 (L1 (L0 L))))

Now you imagine e.g. that

    #0110

compiles to

    (#) l0110


Records
=======

Avoid proliferation of record types via nested tuple analog:

> data Rec :: List TLabel -> List * -> * where
>   RNil  :: Rec Nil Nil
>   RCons :: Label     l     ->    t
>         -> Rec         ls          ts
>         -> Rec (Cons l ls) (Cons t ts)

Record projections (seem to help GHC type check the `Has` instances ...):

> pi1 :: Rec (Cons l ls) (Cons t ts) -> Label l
> pi2 :: Rec (Cons l ls) (Cons t ts) -> t
> pi3 :: Rec (Cons l ls) (Cons t ts) -> Rec ls ts
> pi1 (RCons l _ _) = l
> pi2 (RCons _ t _) = t
> pi3 (RCons _ _ r) = r

Define `Has` instances once and for all:

> instance
>   Has l (Rec (Cons l ls) (Cons t ts)) t where
>     (#) _ = Lens get' upd' where
> --    get'   (RCons _ t _) = t
> --    upd' f (RCons l t r) = RCons l (f t) r
>       get'   r = pi2 r
>       upd' f r = RCons (pi1 r) (f $ pi2 r) (pi3 r)

> instance (Has l (Rec ls ts) t) =>
>   Has l (Rec (Cons l' ls) (Cons t' ts)) t where
>     (#) l = Lens get' upd' where
> --    get'   (RCons _ _ r) = get ((#) l) r
> --    upd' f (RCons l t r) = RCons l t (upd ((#) l) f r)
>       get'   r = get ((#) l) (pi3 r)
>       upd' f r = RCons (pi1 r) (pi2 r) (upd ((#) l) f (pi3 r))

Examples
========

Some labels:

> l0 :: Label (T0 T)
> l0 =         L0 L
> l1 :: Label (T1 T)
> l1 =         L1 L

A record with two fields labeled by `l0` and `l1`:

> type R_0_1 t0 t1 =
>   Rec (Cons (T0 T) (Cons (T1 T) Nil))
>       (Cons t0     (Cons t1     Nil))

Show instances for records are a little tricky, if you want to avoid
putting the `Show` constraint in the `RCons` constructor ...

> type family   All (c:: * -> Constraint) (ts::List *) :: Constraint
> type instance All c Nil         = ()
> type instance All c (Cons t ts) = (c t, All c ts)

> deriving instance All Show ts => Show (Rec ls ts)

Record computations:

> r, r' :: R_0_1 String Bool
> r  = RCons l0 "hello" $ RCons l1 True $ RNil
> r' = upd ((#) l1) not . set ((#) l0) "goodbye" $ r

    ghci> r'
    RCons (L0 L) "goodbye" (RCons (L1 L) False RNil)

The above instances allow overlap. The outermost field is always
chosen:

> r''  = RCons l0 False r
> r''' = upd ((#) l0) not r''

    ghci> r''
    RCons (L0 L) False (RCons (L0 L) "hello" (RCons (L1 L) True RNil))
    ghci> r'''
    RCons (L0 L) True  (RCons (L0 L) "hello" (RCons (L1 L) True RNil))


The End
=======

Prenventing overlap
===================

See AvoidingProliferationOfTypesNoOverlap.lhs for better version.

These mysteriously don't work, but maybe `-XInstanceSigs` from GHC 7.6
would help?  Seems to be some problem with GHC not figuring out how
the types in the local get' and upd' definitions relate to the types
in the instance decl (and in particular the recursive constraint in
the second instance).

Why do the projections above ('pi1', 'pi2', 'pi3') fix the problem?
The idea is that these make signatures clear? I could factor out the
whole 'get' and 'upd' defs maybe, with the same effect?


 > instance
 >   Has l (Rec (Cons l' ls) (Cons t ts)) t where
 >     (#) _ = Lens get upd where
 >       get   (RCons _ t _) = t
 >       upd f (RCons l t r) = RCons l (f t) r

 Stuck here: GHC can't figure out that the tail of the rec has
 the type in the constraint :P

 XXX: Might be related to ty vars in the instance decl not being 
 the same as the ty vars in the instance function defs ...

 > instance (Has l (Rec ls ts) t) =>
 >   Has l (Rec (Cons l' ls) (Cons t' ts)) t where
 >     (#) l = Lens get' upd' where
 >       get' :: (Rec (Cons l' ls) (Cons t' ts)) -> t
 >       get'   (RCons _ _ r) = get ((#) l) r
 >       upd' :: (t -> t) -> (Rec (Cons l' ls) (Cons t' ts))
 >                        -> (Rec (Cons l' ls) (Cons t' ts))
 >       upd' f (RCons l t r) = RCons l t (upd ((#) l) f r)


Preventing overlap:

In practice we may want to prevent overlap (and, unrelated but more
importantly, compare records for equality).  Actually, I don't think I
buy this: we don't actually write these hideous records by hand, but
rather, the compiler creates them for us, and it can order the fields
and prevent duplicates.

Moreover, the implementation below is misquided: what we really want
to avoid is even building a duplicate-label record in the first
place. Below approach just tries to avoid having overlapping 'Has'
instances for them, which doesn't solve any problems.

But, whatever, it's an excuse to do more type hacking ... without
further ado.

> type family   TLabelEq (t :: TLabel) (t' :: TLabel) :: Bool
> type instance TLabelEq T             T              =  True
> type instance TLabelEq (T0 t)        (T0 t')        =  TLabelEq t t'
> type instance TLabelEq (T1 t)        (T1 t')        =  TLabelEq t t'
> type instance TLabelEq (T0 t)        (T1 t')        =  False
> type instance TLabelEq (T1 t)        (T0 t')        =  False



Copy-and-paste and search-and-replace party:

> data Rec' :: List TLabel -> List * -> * where
>   RNil'  :: Rec' Nil Nil
>   RCons' :: Label      l     ->    t
>          -> Rec'         ls          ts
>          -> Rec' (Cons l ls) (Cons t ts)

Rec'ord projections (seem to help GHC type check).

> pi'1 :: Rec' (Cons l ls) (Cons t ts) -> Label l
> pi'2 :: Rec' (Cons l ls) (Cons t ts) -> t
> pi'3 :: Rec' (Cons l ls) (Cons t ts) -> Rec' ls ts
> pi'1 (RCons' l _ _) = l
> pi'2 (RCons' _ t _) = t
> pi'3 (RCons' _ _ r) = r


> instance (TLabelEq l l' ~ True) => 
>   Has l (Rec' (Cons l' ls) (Cons t ts)) t where
>     (#) _ = Lens get' upd' where
>       get'   r = pi'2 r
>       upd' f r = RCons' (pi'1 r) (f $ pi'2 r) (pi'3 r)

> instance (TLabelEq l l' ~ False,
>           Has l (Rec' ls ts) t) =>
>   Has l (Rec' (Cons l' ls) (Cons t' ts)) t where
>     (#) l = Lens get' upd' where
>       get'   r = get ((#) l) (pi'3 r)
>       upd' f r = RCons' (pi'1 r) (pi'2 r) (upd ((#) l) f (pi'3 r))

> deriving instance All Show ts => Show (Rec' ls ts)

Record computations:

> type R_0_1' t0 t1 =
>   Rec' (Cons (T0 T) (Cons (T1 T) Nil))
>        (Cons t0     (Cons t1     Nil))

> rr, rr' :: R_0_1' String Bool
> rr  = RCons' l0 "hello" $ RCons' l1 True $ RNil'
> rr' = undefined

 > rr' = upd ((#) l1) not . set ((#) l0) "goodbye" $ rr

The old def (above) causes an unexpected error now:

    Couldn't match type `[Char]' with `Bool'
    When using functional dependencies to combine
      Has l (Rec' (Cons TLabel l' ls) (Cons * t ts)) t,
        arising from the dependency `l r -> t'
        in the instance declaration at /home/collins/v/haskell-records.git/AvoidingProliferationOfTypes.lhs:220:12
      Has
        (T1 'T)
        (Rec'
           (Cons TLabel (T0 'T) (Cons TLabel (T1 'T) (Nil TLabel)))
           (Cons * [Char] (Cons * Bool (Nil *))))
        Bool,
        arising from a use of `#'
        at /home/collins/v/haskell-records.git/AvoidingProliferationOfTypes.lhs:246:14-16
    In the first argument of `upd', namely `((#) l1)'
    In the first argument of `(.)', namely `upd ((#) l1) not'
    Failed, modules loaded: none.

Whereas the version we wanted to eliminate still works (i.e. the
solution is wrong, independent of the weird failure above):

> rr'' = upd ((#) l0) not $ RCons' l0 False rr

Of course, this makes sense: there is no overlapping instance here :P


Boilerplate:

XXX: I really need to put these defs in a separate file so don't have
to type them over and over ...

> data Lens r t = Lens { get :: r -> t
>                      , upd :: (t -> t) -> (r -> r)
>                      }
> set :: Lens r t -> t -> (r -> r)
> set l c r = upd l (const c) r

> data List a = Nil | Cons a (List a)
