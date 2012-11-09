> {-# LANGUAGE MultiParamTypeClasses
>   , FunctionalDependencies
>   , FlexibleInstances
>   , KindSignatures
>   , DataKinds
>   , GADTs
>   , TypeFamilies
>   , ConstraintKinds
>   #-}

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



Can we avoid the proliferation of record types as well?  Seems
complicated to enforce non-duplication of labels, and label ordering,
but probably Oleg knows how ...



Boilerplate:

XXX: I really need to put the lens defs in a separate file so don't
have to type them over and over ...

> data Lens r t = Lens { get :: r -> t
>                      , upd :: (t -> t) -> (r -> r)
>                      }