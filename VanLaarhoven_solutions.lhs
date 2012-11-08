Based on http://twanvl.nl/blog/haskell/cps-functional-references and
http://r6.ca/blog/20120623T104901Z.html

> {-# LANGUAGE Rank2Types #-}
> import Control.Applicative
> import Control.Monad.Identity

Part 1:

A 'Lens a b' lets us see a 'b' inside an 'a'.

We can get the 'b':

> get :: Lens a b -> a -> b

We can modify the 'b':

> modify :: Lens a b -> (b -> b) -> a -> a

And we can replace the 'b':

> set :: Lens a b -> b -> a -> a



Exercise 1:

(a) Define 'get', 'modify', and 'set', with the types given above, for
the type 'Lens' defined by:

> type Lens a b =
>   forall f. Functor f => (b -> f b) -> (a -> f a)

The main thing here is figuring out how to instantiate the 'Functor f'.

(b) Test your definitions of 'get', 'modify', and 'set', by defining:

> l1 :: Lens (x,y) x
> l2 :: Lens (x,y) y

s.t. 'test1' evaluates to 'True':

> test1 =
>   get    l1              t == 1 &&
>   get    l2              t == "hello" &&
>   modify l2 (++" world") t == (1,"hello world") &&
>   set    l1 0            t == (0,"hello")
>  where
>   t = (1,"hello")



Part 2:

A 'PLens a b a' b'' is like 'Lens', but it allows polymorphic
updates: it lets us see a 'b' inside an 'a', and lets us modify or
replace the 'b' with a 'b'', resulting in an 'a''.

We can get the 'b':

> pget :: PLens a b a' b' -> a -> b

We can replace the 'b' with a 'b'' computed from 'b':

> pmodify :: PLens a b a' b'-> (b -> b') -> a -> a'

And we can replace the 'b' with a fixed 'b'':

> pset :: PLens a b a' b' -> b' -> a -> a'



Exercise 2:

(a) Define 'pget', 'pmodify', and 'pset', with the types given above,
for the type 'PLens' defined by:

> type PLens a b a' b' =
>   forall f. Functor f => (b -> f b') -> (a -> f a')

(b) Test your definitions of 'pget', 'pmodify', and 'pset', by
defining:

> pl1 :: PLens (x,y) x (x',y) x'
> pl2 :: PLens (x,y) y (x,y') y'

s.t. 'test2' evaluates to 'True':

> test2 =
>   pget    pl1        t == 1 &&
>   pget    pl2        t == "hello" &&
>   pmodify pl2 length t == (1,5) &&
>   pset    pl1 "Ot"   t == ("Ot","hello")
>  where
>   t = (1,"hello")



SOLUTION 1:

> get    = pget
> modify = pmodify
> set    = pset
> l1     = pl1
> l2     = pl2

:D

SOLUTION 2:

> pget    l   = getConst . l Const
> pmodify l m = runIdentity . l (Identity . m)
> pset    l b = pmodify l (const b)
> pl1 f (x,y) = fmap (\x' -> (x',y)) (f x)
> pl2 f (x,y) = fmap (\y' -> (x,y')) (f y)

