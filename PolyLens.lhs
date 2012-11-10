> {-# LANGUAGE MultiParamTypeClasses
>   , FunctionalDependencies
>   , FlexibleInstances
>   #-}

Lens for polymorphic update
===========================

In Haskell we can def:

    data Pair a b = Pair { pi1 :: a, pi2 :: b }

and then do polymorphic updates:

    p :: Pair Int Int
    p = Pair 1 2
    p { pi1 = "hello world" } :: Pair String Int

but we can't do this with the lenses presented so far.

View `r` as `t` and (polymorphically) update `t` to `t'` yielding
`r'`:

> data PolyLens r t r' t' =
>   PolyLens { get :: r -> t
>            , upd :: (t -> t') -> (r -> r')
>            }

<!--
No way to make 'PolyLens' a 'Control.Category'?  Maybe they are
"arrows"?
-->

Category stuff
--------------

Identity:

> id_PolyLens :: PolyLens r r r' r'
> id_PolyLens = PolyLens { get = id
>                        , upd = id
>                        }

Compose:

> compose_PolyLens
>   :: PolyLens b c b' c' -> PolyLens a b a' b'
>   -> PolyLens a c a' c'

    upd2 :: (c -> c') -> (b -> b')
    upd1 :: (b -> b') -> (a -> a')
    upd1 . upd2 :: (c -> c') -> (a -> a')

> compose_PolyLens (PolyLens get2 upd2) (PolyLens get1 upd1)
>   = PolyLens { get = (get2 . get1)
>              , upd = (upd1 . upd2)
>              }

<!--
Copied and modified from 'Records.lhs' ... maybe better to go whole hog and use
single class paramed by label here?
-->

`Has` classes
=============

Functional dependencies get fancier:

> class Has_1 r t r' t' | r -> t , r t t' -> r' where
>   _1 :: PolyLens r t r' t'
> class Has_2 r t r' t' | r -> t , r t t' -> r' where
>   _2 :: PolyLens r t r' t'

But `instance` defs are the same (search and replace oriented
programming):

> instance Has_1 (t1,t2) t1 (t1',t2) t1' where
>   _1 = PolyLens get upd where
>     get   (x1,x2) = x1 -- fst
>     upd f (x1,x2) = (f x1,x2)
> instance Has_2 (t1,t2) t2 (t1,t2') t2' where
>   _2 = PolyLens get upd where
>     get   (x1,x2) = x2 -- snd
>     upd f (x1,x2) = (x1,f x2)

> newtype T_x_y tx ty = T_x_y (tx, ty)
>   deriving Show

> t_x_y :: PolyLens (T_x_y tx ty) (tx, ty) (T_x_y tx' ty') (tx', ty')
> t_x_y = PolyLens get upd where
>   get   (T_x_y t) = t
>   upd f (T_x_y t) = T_x_y $ f t

> class Has_x r t r' t' | r -> t , r t t' -> r' where
>   _x :: PolyLens r t r' t'
> class Has_y r t r' t' | r -> t , r t t' -> r' where
>   _y :: PolyLens r t r' t'

> instance Has_x (T_x_y tx ty) tx (T_x_y tx' ty) tx' where
>   _x = _1 `compose_PolyLens` t_x_y
> instance Has_y (T_x_y tx ty) ty (T_x_y tx ty') ty' where
>   _y = _2 `compose_PolyLens` t_x_y

Example
=======

Boom goes the dynamite!

> main = do
>   let t = T_x_y (1,2)
>   print t
>   print $ upd _x show t

Prints:

    T_x_y (1  ,2)
    T_x_y ("1",2)



The End
=======




Notes
=====

Maybe better to make the type:

    data PolyLens r t t' r' = ...

which can be read as: 'r' can be viewed as 't', and if you change 't'
to 't'', then 'r' changes to 'r'' ... sure would be nice to just give
a partially applied type function:

    data PolyLens f t t' =
      PolyLens { get :: f t -> t
               , upd :: (t -> t') -> (f t -> f t')
               }


    -- And what happens to the fundep ???
    class Has l f t t' where
      (#) :: l -> PolyLens f t t'

but current haskell won't let us use this.  E.g. we can't write

    instance Has L1 (\t1 -> (t1, t2)) t1 t1'

because we can't write

    (\t1 -> (t1, t2))

at type level.
