% Haskell Records
% Nathan Collins <nathan.collins@gmail.com>
% 9 July 2012

> {-# LANGUAGE MultiParamTypeClasses
>   , FunctionalDependencies
>   , FlexibleInstances
>   , NamedFieldPuns
>   #-}
> module Records where
> 
> import Prelude hiding (mod, (.), id)
> import Control.Category
> 

Current Records System
======================

The current Haskell records system supports named fields, but field
names can't be re-used, and so functions that access fields by name
can't be polymorphic in the records they act on.

Features
--------

Fields can be named:

> data Point2D n = Point2D { x, y :: n }
>                  deriving Show

instead of just using position:

> data Point2D' n = Point2D' n n

We construct points without remembering order of `x` and `y`:

> p :: Point2D Int
> p = Point2D { y = 4, x = 3 }

An accessor is automatically created for each field, with the same
name as the field:

    ghci> :t x
    x :: Point2D n -> n

Records can be updated using the `record { field = newValue }` syntax:

> shiftXBy, shiftXBy' :: Num n => n -> Point2D n -> Point2D n
> shiftXBy dx p = p { x = x p + dx }

Record fields can also be pattern-matched by name, and constructed
without using names, by giving the arguments in declaration order:

> shiftXBy' dx (Point2D { x, y }) = Point2D (x + dx) y

For example:

    ghci> shiftXBy (-3) p
    Point2D {x = 0, y = 4}

Problems
--------

### Field names can't be reused

The declaration
 
    data Point3D n = Point3D { x, y, z :: n }

causes the error ``Multiple declarations of `x'``, because `x` is
already defined as an accessor for `Point2D`.

(Actually, some limited reuse is allowed.  A name can be reused in
different constructors of the same ADT:

> data FileSystemObj = File { name :: String, bytes :: String }
>                    | Dir  { name :: String, contents :: [FileSystemObj] }

... but only if the types are the same. The declaration

    data FileSystemObj' = File' { name' :: String, contents' :: String }
                        | Dir'  { name' :: String, contents' :: [FileSystemObj] }

causes another accessor related error:

    Couldn't match expected type `Char'
                with actual type `FileSystemObj'
    Expected type: String
      Actual type: [FileSystemObj]
    In the expression: contents'
    In an equation for contents':
        contents' (Dir' {contents' = contents'}) = contents'

)

### Functions can't be polymorphic in record fields

One can imagine a more general type for `shiftByX`, that works for
anything with a numeric `x` field:

    shiftByX :: (Num n, r { x :: n }) => n -> r -> r

And the definition shouldn't have to change:

    shiftByX dx p = r { x = x p + dx }

### Nested updates are a pain

It's natural to nest records inside other records:

> data Shape n = Rectangle { center :: Point2D n
>                          , width, height :: n
>                          }
>              | Circle    { center :: Point2D n
>                          , radius :: n
>                          }
>              deriving Show

But then updates are awkward:

> shiftYBy :: (Num n) => n -> Shape n -> Shape n
> shiftYBy dy p = p { center = (center p) { y = (y . center) p + dy } }

For example:

    ghci> shiftYBy 1 (Circle { center = p, radius = 10 } )
    Circle {center = Point2D {x = 3, y = 5}, radius = 10}

Solution: Type-classes and Lenses
=================================

Motivation
----------

Current Haskell records don't let us reuse field names because the
corresponding accessor functions conflict.  For example, above we
couldn't have `Point2D` and `Point3D` both with an `x` field, because
then the accessor function `x` would have two incompatible types:

    x :: Point2D n -> n
    x :: Point3D n -> n

What do we do in Haskell when we want two different functions with
the same name? Make a type class!

Haskell type classes let us write *ad-hoc* polymorphic functions, that
is, functions with the same name, but which treat different types
differently (like classes in Java).  Ad-hoc polymorphism is contrasted
with *parametric* polymorphism, where we write a single function that
works at many types, by treating all types uniformly (like generics in
Java).

For example, their is a standard type class `Eq` for equality
functions:

    class Eq a where
      (==) :: a -> a -> Bool
      (/=) :: a -> a -> Bool
      x == y = not (x /= y)
      x /= y = not (x == y)

And we could define equality on `Point2D` by:

> eqPoint2D :: Eq n => Point2D n -> Point2D n -> Bool
> Point2D { x = x1, y = y1 } `eqPoint2D` Point2D { x = x2, y = y2 } =
>   x1 == x2 && y1 == y2

> instance Eq n => Eq (Point2D n) where
>   (==) = eqPoint2D

Of course, we could have derived that `Eq` instance automatically.

Type-class for Fields
---------------------

Make the field accessors be type-class functions.  One class per field
name is simple.  For example:

    class Has_x r t where _x :: r -> t

But actually, let's make the accessors more flexible:

> class Has_x r t | r -> t where _x :: Lens r t
> class Has_y r t | r -> t where _y :: Lens r t
> class Has_z r t | r -> t where _z :: Lens r t

For now, just think of a `Lens r t` as something that lets us access
`t`s inside of `r`s.

The `Has_*` classes have two parameters, `r` and `t`.  I don't know if
this has an analog is Java.  I think it's called a "multi method" in
general OOP.

Also, note the *functional dependency*: `| r -> t`.  It means that `t`
is a function of `r`.  We'll come back to this later.

Lens Data Type
--------------

A `Lens a b` lets us view an `a` as a `b`:

> data Lens a b = Lens (a -> b)               -- get
>                      ((b -> b) -> (a -> a)) -- mod

A production implementation might keep the lens data type abstract and
provide an interface via `get`, `set`, and `mod`:

> get :: Lens a b -> a -> b
> get (Lens g _) = g
> 
> mod :: Lens a b -> (b -> b) -> (a -> a)
> mod (Lens _ m) = m
> 
> set :: Lens a b -> b -> a -> a
> set l x = mod l (const x)

... and some way to create lenses:

> fromGetMod :: (a -> b) -> ((b -> b) -> (a -> a)) -> Lens a b
> fromGetMod = Lens
>
> fromGetSet :: (a -> b) -> (b -> (a -> a)) -> Lens a b
> fromGetSet get set = Lens get mod where
>   mod f x = set (f $ get x) x

Lenses are like Functions
-------------------------

We can compose lenses:

> composeLens :: Lens b c -> Lens a b -> Lens a c
> l1 `composeLens` l2 = Lens (get l1 . get l2) (mod l2 . mod l1)

And there is an identity lens:

> idLens :: Lens a a
> idLens = Lens id id

In fact, there's a type class for function-like things:

> instance Category Lens where
>   id  = idLens
>   (.) = composeLens

Examples
========



Record Implementation: newtypes and tuples
==========================================



Fields should be lexicographically sorted, so that a record type is
uniquely determined by the labels and types, independent of the
order the labels are given.

Tuple in a newtype
------------------

One `newtype T_<l1>_..._<ln>` for each record `{l1:t1,...,ln:tn}`
in the program.

> newtype T_x_y_z tx ty tz = T_x_y_z (tx, ty, tz)

A lens into the wrapped tuple.  This facilitates concise `Has_*`
instances for the labels.

> t_x_y_z :: Lens (T_x_y_z tx ty tz) (tx, ty, tz)
> t_x_y_z = Lens get mod where
>   get   (T_x_y_z t) = t
>   mod f (T_x_y_z t) = T_x_y_z $ f t
> 

Records with one field are special.  The newtype here is the same:

> newtype T_x tx = T_x tx
> t_x :: Lens (T_x tx) tx
> t_x = Lens get mod where
>   get   (T_x t) = t
>   mod f (T_x t) = T_x $ f t

, but the has-instance is degenerate, since the "tuple lens" `t_x`
is already the `Has_x` instance:

> instance Has_x (T_x tx) tx where
>   _x = _1 . t_x where _1 = id

`Has_*` instances
-----------------


For tuples.

> class Has_1 r t | r -> t where _1 :: Lens r t
> class Has_2 r t | r -> t where _2 :: Lens r t
> class Has_3 r t | r -> t where _3 :: Lens r t


> instance Has_x (T_x_y_z tx ty tz) tx where
>   _x = _1 . t_x_y_z
> instance Has_y (T_x_y_z tx ty tz) ty where
>   _y = _2 . t_x_y_z
> instance Has_z (T_x_y_z tx ty tz) tz where
>   _z = _3 . t_x_y_z

We use `t_x_y_z`, the lens into the underlying tuple, but we could
also write the instances directly, e.g.:

> {-
> instance Has_x (T_x_y_z tx ty tz) tx where
>   _x = Lens get' mod' where
>     get'   (T_x_y_z t) = get _1 t
>     mod' f (T_x_y_z t) = T_x_y_z $ mod _1 f t
> -}

Tuple `Has_*` instances
=======================

1-tuples
--------

Haskell doesn't have 1-tuples and an identity instance would
conflict with all other instances:

> {-
> instance Has_1 t1 t1 where
>   _1 = id
> -}

2-tuples
--------

> instance Has_1 (t1,t2) t1 where
>   _1 = Lens get mod where
>     get   (x1,x2) = x1 -- fst
>     mod f (x1,x2) = (f x1,x2)
> instance Has_2 (t1,t2) t2 where
>   _2 = Lens get mod where
>     get   (x1,x2) = x2 -- snd
>     mod f (x1,x2) = (x1,f x2)
> 

3-tuples
--------

> instance Has_1 (t1,t2,t3) t1 where
>   _1 = Lens get mod where
>     get   (x1,x2,x3) = x1
>     mod f (x1,x2,x3) = (f x1,x2,x3)
> instance Has_2 (t1,t2,t3) t2 where
>   _2 = Lens get mod where
>     get   (x1,x2,x3) = x2
>     mod f (x1,x2,x3) = (x1,f x2,x3)
> instance Has_3 (t1,t2,t3) t3 where
>   _3 = Lens get mod where
>     get   (x1,x2,x3) = x3
>     mod f (x1,x2,x3) = (x1,x2,f x3)
> 

4-tuples, 5-tuples, ...

Examples
========

Shape, point2d, piont3d