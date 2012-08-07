{-# LANGUAGE RankNTypes
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
  #-}

-- A potential issue raised by Mark Jones: does the fundep in the has
-- class precludes higher rank fields?
--
-- Conclusion: Mark is right that there are problems, but it's not
-- clear that removing the fundep makes them go away.
--
-- Would be good to have a practical example of this.

data Lens a b = Lens (a -> b) ((b -> b) -> (a -> a))

class Has_x r t where
-- class Has_x r t | r -> t where
  _x :: Lens r t

data Foo = Foo { x :: forall a. a -> a }
-- This instance is never allowed:
{-
instance Has_x Foo (forall a. a -> a)
-}
-- With the fundep, this instance is not allowed:
instance Has_x Foo (a -> a) where
{-
    Illegal polymorphic or qualified type: forall a. a -> a
    In the instance declaration for `Has_x Foo (forall a. a -> a)'
-}
-- Without the fundep, that instance is allowed, but I can't figure
-- out how to define it, getting errors about trying match against
-- rigid type variables.  Also, it's not clear that it's a the right
-- instance type either ...
  _x = Lens g m where
    g   (Foo x) = x
    -- Adding a signature here just leads to different rigid-tyvar
    -- errors.  And probably the type is wrong anyway: should be
    -- higher rank?
    m :: ((a -> a) -> (a -> a)) -> (Foo -> Foo)
    m f (Foo x) = Foo $ f x