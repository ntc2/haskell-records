-- Based on:
-- http://stackoverflow.com/questions/6939043/is-it-possible-to-place-inequality-constraints-on-haskell-type-variables

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- The following code is my own hacked modifications to Oleg's original TypeEq. Note
-- that his TypeCast class is no longer needed, being basically equivalent to ~.

data Yes = Yes deriving (Show)
data No = No deriving (Show)

class (TypeEq x y No) => (:/~) x y
instance (TypeEq x y No) => (:/~) x y

class (TypeEq' () x y b) => TypeEq x y b where

instance (TypeEq' () x y b) => TypeEq x y b where

class TypeEq' q x y b | q x y -> b where

instance (b ~ Yes) => TypeEq' () x x b where

instance (b ~ No) => TypeEq' q x y b where

const' :: (a :/~ b) => a -> b -> a
const' x _ = x

a = const' () True
-- Rejected because types are equal!
{- b = const' () () -}
