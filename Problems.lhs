> {-# LANGUAGE RankNTypes
>            , MultiParamTypeClasses
>            , FunctionalDependencies
>            , FlexibleInstances
>            #-}

1) Higher rank fields aren't supported.  E.g. this is OK:

> data HRF = MkHRF { unHRF :: forall a. a -> a }

But we can't write the corresponding 'Has' instance:

> class Has l r t | l r -> t where
>   (#) :: l -> Lens r t
>
> data Lens r t = MkLens { get :: r -> t
>                        , set :: t -> r -> r
>                        }
>
> data LunHRF = LunHRF
>
> instance Has LunHRF HRF (forall a. a -> a) where
>   (#) _ = MkLens g s where
>     g = unHRF
>     s = const MkHRF

It's not clear that there's anything fundamental going on here ...

2) Can't hide lenses when trying to keep a data type (say in a
library) abstract. This is because type class instances are global in
Haskell.  I claim this is a problem with Haskell.  The simple
solution, which is good enough here, is to simply allow type class
instances to be marked "unexported": they can't be referred to where
ever they are unexported.  Should not be hard to implement.  Note that
I'm *not* proposing that multiple overlapping instances should be
definable by different modules.  That's still an error.  I'm just
saying the instances should be inaccessible in some places, in the
same way that constructors for an abstract data type may be.