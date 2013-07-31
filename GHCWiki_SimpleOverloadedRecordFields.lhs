> {-# LANGUAGE KindSignatures
>            , DataKinds
>            , MultiParamTypeClasses
>            , FlexibleInstances
>            , FunctionalDependencies
>            #-}
>
> import GHC.TypeLits

The wiki
<http://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields>
claims that supporting type changing updates adds "much more
complexity".  I don't buy this.  They start with (roughly: I added 
explicit field parameters and changed some names):

> data Field (l :: Symbol) = F
>
> class Get (r :: *) (f :: Symbol) (t :: *) where
>   get :: Field f -> r -> t
> 
> class Set (r :: *) (f :: Symbol) (t :: *) where
>   set :: Field f -> t -> r -> r

and give a data type:

> data R a = R { x :: a }
>
> instance Get (R a) "x" a where
>   get _ (R a) = a
>
> instance Set (R a) "x" a where
>   set _ a (R _) = R a

and then point out that type-changing update is not supported by their
'Set' class:

    setX :: R Int -> R Bool
    setX r = r { x = True }

and then say that 'set' needs "much more complexity" to support type
changing.

However, as is well known from lenses, we just need more type
parameters (and fundeps, but I think we want those for the earlier
versions to, for type inference to work):

> class Set' (r :: *) (f :: Symbol) (t' :: *) (r' :: *) | r f t' -> r' where
>   set' :: Field f -> t' -> r -> r'
>
> instance Set' (R a) "x" a' (R a') where
>   set' _ a' (R _) = R a'
>
> setX :: R Int -> R Bool
> setX r = set' (F::Field "x") True r
>       -- r { x = True }

Finally, they point out that iterated 'set' may not work when the
record field types are non-linear in the record's type parameters:

    data S a = S { y,z :: a }
    setYZ :: S Int -> S Bool
    setYZ s = s { y = True, z = False }

This is true, but not necessarily the end of the world.  It suffices
to introduce a linearized version of 'S':

> data LinearS a b = S { y :: a, z :: b }
> type S a = LinearS a a

Now, any code with a type signature involving 'S' should work exactly
as before. But, we can also do the iterated update:

> instance Set' (LinearS a b) "y" a' (LinearS a' b) where
>   set' _ a' (S _ b) = S a' b
> instance Set' (LinearS a b) "z" b' (LinearS a b') where
>   set' _ b' (S a _) = S a b'
>
> setYZ :: S Int -> S Bool
> setYZ s = set' (F::Field "y") True . set' (F::Field "z") False $ s
>        -- s { y = True , z = False }

The obvious downside here is that the type of 'set'' is now in terms
of 'LinearS', and type inference will infer 'LinearS', and not 'S',
for unannotated programs.
