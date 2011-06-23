-- Author: Federico Mastellone (fmaste@gmail.com)

-- Functor Collection classes.
-- TODO: Make it haddock compatible!

-- TODO: Wait for equality constraints in superclass contexts (c ~ FunctorType c a) to implement 
-- http://www.haskell.org/ghc/docs/7.0.3/html/users_guide/type-families.html#id636192

-- TODO: Google how to make Data.Set a Functor (Constraint Families, Class families, Restricted data types)

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
module Data.Collection.Functor (
	Functor(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Prelude hiding (Functor)

-- CLASSES
-------------------------------------------------------------------------------

-- Functor class for Collections.
class Functor f a b where
	type FunctorType f :: * -> *

	map :: (a -> b) -> FunctorType f a -> FunctorType f b

