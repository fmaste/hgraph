-- Author: Federico Mastellone (fmaste@gmail.com)

-- Functor Collection classes.
-- TODO: Make it haddock compatible!

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

