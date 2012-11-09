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

- Records which enforce label uniqueness:

> data Rec :: List TLabel -> List * -> * where
>   RNil  :: Rec Nil Nil
>   RCons :: TLabelNotIn l ls
>         => Label     l     ->    t
>         -> Rec         ls          ts
>         -> Rec (Cons l ls) (Cons t ts)

XXX: above 'Rec' def causes a "GHC internal error":

    GHC internal error: `TLabelNotIn' is not in scope during type checking, but it passed the renamer
        tcl_env of environment: [(a3xD, AThing k_a3B3),
                                 (a3xE, AThing k_a3B4), (a3xF, AThing k_a3B5),
                                 (a3xG, AThing k_a3B6), (r9W, AThing List TLabel -> List * -> *),
                                 (r9X, ANothing), (r9Y, ANothing)]
        In the definition of data constructor `RCons'
        In the data type declaration for `Rec'
    Failed, modules loaded: none.

Googling for

    GHC internal error is not in scope during type checking, but it
    passed the renamer

finds many examples of this error msg in relation to template haskell,
but none for vanilla haskell!

Record projections (seem to help GHC type check).

> pi1 :: Rec (Cons l ls) (Cons t ts) -> Label l
> pi2 :: Rec (Cons l ls) (Cons t ts) -> t
> pi3 :: Rec (Cons l ls) (Cons t ts) -> Rec ls ts
> pi1 (RCons l _ _) = l
> pi2 (RCons _ t _) = t
> pi3 (RCons _ _ r) = r

'Has' instances:

> instance
>   Has l (Rec (Cons l ls) (Cons t ts)) t where
>     (#) _ = Lens get' upd' where
>       get'   r = pi2 r
>       upd' f r = RCons (pi1 r) (f $ pi2 r) (pi3 r)

> instance (Has l (Rec ls ts) t) =>
>   Has l (Rec (Cons l' ls) (Cons t' ts)) t where
>     (#) l = Lens get' upd' where
>       get'   r = get ((#) l) (pi3 r)
>       upd' f r = RCons (pi1 r) (pi2 r) (upd ((#) l) f (pi3 r))

Examples:

- Some labels:

> l0 :: Label (T0 T)
> l0 =         L0 L
> l1 :: Label (T1 T)
> l1 =         L1 L

- A record with two fields labeled by 'l0' and 'l1':

> type R_0_1 t0 t1 =
>   Rec (Cons (T0 T) (Cons (T1 T) Nil))
>       (Cons t0     (Cons t1     Nil))

- 'Show' instances:

Show instances for records are a little tricky, if you want to avoid
putting the 'Show' constraint in the 'RCons' constructor ...

> type family   All (c:: * -> Constraint) (ts::List *) :: Constraint
> type instance All c Nil = ()
> type instance All c (Cons t ts) = (c t, All c ts)

> deriving instance All Show ts => Show (Rec ls ts)

- Record computations:

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



Boilerplate:

XXX: I really need to put these defs in a separate file so don't have
to type them over and over ...

> data Lens r t = Lens { get :: r -> t
>                      , upd :: (t -> t) -> (r -> r)
>                      }
> set :: Lens r t -> t -> (r -> r)
> set l c r = upd l (const c) r

> data List a = Nil | Cons a (List a)
