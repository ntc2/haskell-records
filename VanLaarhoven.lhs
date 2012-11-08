Based on http://twanvl.nl/blog/haskell/cps-functional-references and
http://r6.ca/blog/20120623T104901Z.html.  Spoiler alert: all the
exercise solutions can be found on those pages.

> {-# LANGUAGE Rank2Types #-}

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

Hint: The main thing here is figuring out how to instantiate the
'Functor f'.  Standard library (non 'Prelude') functors suffice ...

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

> get    l   a = undefined
> modify l m a = undefined
> set    l b a = undefined
> l1 f (x,y)   = undefined
> l2 f (x,y)   = undefined

SOLUTION 2:

> pget    l   a = undefined
> pmodify l m a = undefined
> pset    l b a = undefined
> pl1 f (x,y)   = undefined
> pl2 f (x,y)   = undefined
