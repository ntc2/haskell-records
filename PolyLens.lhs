> {-# LANGUAGE MultiParamTypeClasses
>   , FunctionalDependencies
>   , FlexibleInstances
>   #-}


> data PolyLens r t r' t' =
>   PolyLens { get :: r -> t
>            , upd :: (t -> t') -> (r -> r')
>            }

No way to make 'PolyLens' a 'Control.Category'?  Maybe they are
"arrows"?

> id_PolyLens :: PolyLens r r r' r'
> id_PolyLens = PolyLens { get = id
>                        , upd = id
>                        }

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


Copied from 'Records.lhs' ... maybe better to go whole hog and use
single class paramed by label here?

> class Has_1 r t r' t' | r -> t , r t t' -> r' where
>   _1 :: PolyLens r t r' t'
> class Has_2 r t r' t' | r -> t , r t t' -> r' where
>   _2 :: PolyLens r t r' t'

> instance Has_1 (t1,t2) t1 (t1',t2) t1' where
>   _1 = PolyLens get mod where
>     get   (x1,x2) = x1 -- fst
>     mod f (x1,x2) = (f x1,x2)
> instance Has_2 (t1,t2) t2 (t1,t2') t2' where
>   _2 = PolyLens get mod where
>     get   (x1,x2) = x2 -- snd
>     mod f (x1,x2) = (x1,f x2)

A record just maps field names onto the underlying tuple components:

(search and replace oriented programming)

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

Boom goes the dynamite!

> main = do
>   let t = T_x_y (1,2)
>   print t
>   print $ upd _x show t










Notes:

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
