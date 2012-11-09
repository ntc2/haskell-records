> {-# LANGUAGE MultiParamTypeClasses
>   , FunctionalDependencies
>   , FlexibleInstances
>   , KindSignatures
>   , DataKinds
>   , GADTs
>   , TypeFamilies
>   , ConstraintKinds
>   -- , InstanceSigs
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

> data Rec :: List TLabel -> List * -> * where
>   RNil  :: Rec Nil Nil
>   RCons :: Label     l     ->    t
>         -> Rec         ls          ts
>         -> Rec (Cons l ls) (Cons t ts)

Record projections (seem to help GHC type check).

> pi1 :: Rec (Cons l ls) (Cons t ts) -> Label l
> pi2 :: Rec (Cons l ls) (Cons t ts) -> t
> pi3 :: Rec (Cons l ls) (Cons t ts) -> Rec ls ts
> pi1 (RCons l _ _) = l
> pi2 (RCons _ t _) = t
> pi3 (RCons _ _ r) = r

> instance (l ~ l') =>
>   Has l (Rec (Cons l' ls) (Cons t ts)) t where
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


These mysteriously don't work, but maybe '-XInstanceSigs' from GHC 7.6
would help?  Seems to be some problem with GHC not figuring out how
the types in the local get' and upd' definitions relate to the types
in the instance decl (and in particular the recursive constraint in
the second instance).

Why do the projections above ('pi1', 'pi2', 'pi3') fix the problem?
The idea is that these make signatures clear? I could factor out the
whole 'get' and 'upd' defs maybe, with the same effect?


 > instance (l ~ l') =>
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



Boilerplate:

XXX: I really need to put these defs in a separate file so don't have
to type them over and over ...

> data Lens r t = Lens { get :: r -> t
>                      , upd :: (t -> t) -> (r -> r)
>                      }

> data List a = Nil | Cons a (List a)