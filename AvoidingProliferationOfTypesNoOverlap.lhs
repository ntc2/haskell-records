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

Avoid proliferation of 'Has_*' classes and label types (with 'Has *'
class) by defining singleton types for label strings.  This encoding
is a special case of a type-level lists and term-level het lists
encoding.



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
same way a for finite tuple encodings.

Avoid repeated labels:

- Label equality:

> type family   TLabelEq (t :: TLabel) (t' :: TLabel) :: Bool
> type instance TLabelEq T             T              =  True
> type instance TLabelEq (T0 t)        (T0 t')        =  TLabelEq t t'
> type instance TLabelEq (T1 t)        (T1 t')        =  TLabelEq t t'
> type instance TLabelEq (T0 t)        (T1 t')        =  False
> type instance TLabelEq (T1 t)        (T0 t')        =  False

- List not containing a given label:

> type family   TLabelNotIn (t :: TLabel) (ts :: List TLabel) :: Constraint
> type instance TLabelNotIn t Nil          = ()
> type instance TLabelNotIn t (Cons t' ts) = (TLabelEq t t' ~ False
>                                            ,TLabelNotIn t ts)

XXX: this causes a "GHC internal error":

    GHC internal error: `TLabelNotIn' is not in scope during type checking, but it passed the renamer
        tcl_env of environment: [(a3xD, AThing k_a3B3),
                                 (a3xE, AThing k_a3B4), (a3xF, AThing k_a3B5),
                                 (a3xG, AThing k_a3B6), (r9W, AThing List TLabel -> List * -> *),
                                 (r9X, ANothing), (r9Y, ANothing)]
        In the definition of data constructor `RCons'
        In the data type declaration for `Rec'
    Failed, modules loaded: none.

> data Rec :: List TLabel -> List * -> * where
>   RNil  :: Rec Nil Nil
>   RCons :: TLabelNotIn t ts
>         => Label     l     ->    t
>         -> Rec         ls          ts
>         -> Rec (Cons l ls) (Cons t ts)

Record projections (seem to help GHC type check).

> pi1 :: Rec (Cons l ls) (Cons t ts) -> Label l
> pi2 :: Rec (Cons l ls) (Cons t ts) -> t
> pi3 :: Rec (Cons l ls) (Cons t ts) -> Rec ls ts
> pi1 (RCons l _ _) = l
> pi2 (RCons _ t _) = t
> pi3 (RCons _ _ r) = r


> instance
>   Has l (Rec (Cons l ls) (Cons t ts)) t where
>     (#) _ = Lens get' upd' where
> --    get'   (RCons _ t _) = t
> --    upd' f (RCons l t r) = RCons l (f t) r
>       get'   r = pi2 r
>       upd' f r = RCons (pi1 r) (f $ pi2 r) (pi3 r)

> -- Maybe better to have an 'l `NotEq` l'' constraint here, to avoid
> -- overlap.  Can use the tricks from TypeInequality.hs to achieve
> -- this.

> instance (Has l (Rec ls ts) t) =>
>   Has l (Rec (Cons l' ls) (Cons t' ts)) t where
>     (#) l = Lens get' upd' where
> --    get'   (RCons _ _ r) = get ((#) l) r
> --    upd' f (RCons l t r) = RCons l t (upd ((#) l) f r)
>       get'   r = get ((#) l) (pi3 r)
>       upd' f r = RCons (pi1 r) (pi2 r) (upd ((#) l) f (pi3 r))


These mysteriously don't work, but maybe '-XInstanceSigs' from GHC 7.6
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


Examples:

Some labels:

> l0 :: Label (T0 T)
> l0 =         L0 L
> l1 :: Label (T1 T)
> l1 =         L1 L

A record with two fields labeled by 'l0' and 'l1':

> type R_0_1 t0 t1 =
>   Rec (Cons (T0 T) (Cons (T1 T) Nil))
>       (Cons t0     (Cons t1     Nil))

Show instances for records are a little tricky, if you want to avoid
putting the 'Show' constraint in the 'RCons' constructor ...

> type family   All (c:: * -> Constraint) (ts::List *) :: Constraint
> type instance All c Nil = ()
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

Preventing overlap:

In practice we may want to prevent overlap (and, unrelated but more
importantly, compare records for equality).  Actually, I don't think I
buy this: we don't actually write these hideous records by hand, but
rather, the compiler creates them for us, and it can order the fields
and prevent duplicates.

But, whatever, it's an excuse to do more type hacking ... without
further ado.

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

The above instances allow overlap; the outermost field is chosen:

> rr'' = upd ((#) l0) not $ RCons' l0 False rr


Boilerplate:

XXX: I really need to put these defs in a separate file so don't have
to type them over and over ...

> data Lens r t = Lens { get :: r -> t
>                      , upd :: (t -> t) -> (r -> r)
>                      }
> set :: Lens r t -> t -> (r -> r)
> set l c r = upd l (const c) r

> data List a = Nil | Cons a (List a)
