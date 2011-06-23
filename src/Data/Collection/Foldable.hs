-- Author: Federico Mastellone (fmaste@gmail.com)

-- Foldable Collection classes.
-- TODO: Make it haddock compatible!

-- TODO: Wait for equality constraints in superclass contexts (DCF.Element c ~ Element c) to implement 
-- http://www.haskell.org/ghc/docs/7.0.3/html/users_guide/type-families.html#id636192

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Foldable (
	Foldable(..),
	Foldable1(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Prelude hiding (foldr, foldl)
import Data.Maybe (fromMaybe)

-- CLASSES
-------------------------------------------------------------------------------

-- Foldable class to be used with Collections.
class Foldable c where
	type Element c

	-- Right-associative fold over the Elements of a Collection.
	foldr :: (Element c -> a -> a) -> a -> c -> a

	-- Left-associative fold over the Elements of a Collection.
	foldl :: (a -> Element c -> a) -> a -> c -> a

	-- Fold over the Elements of a Collection, associating to the right, but strictly.
	foldr' :: (Element c -> a -> a) -> a -> c -> a
	foldr' = defaultFoldr'

	-- Fold over the Elements of a Collection, associating to the left, but strictly.
	foldl' :: (a -> Element c -> a) -> a -> c -> a
	foldl' = defaultFoldl'

defaultFoldr' :: Foldable c => (Element c -> a -> a) -> a -> c -> a
defaultFoldr' f z0 xs = foldl f' id xs z0 where 
	f' g x z = g $! f x z

defaultFoldl' :: Foldable c => (a -> Element c -> a) -> a -> c -> a
defaultFoldl' f z0 xs = foldr f' id xs z0 where 
	f' x g z = g $! f z x

-------------------------------------------------------------------------------

-- Collections like Map and MapSet won't implement this functions, so they are separated from Foldable.
class Foldable c => Foldable1 c where

	-- A variant of foldr that has no base case.
	-- May only be applied to non-empty Collections.
	foldr1 :: (Element c -> Element c -> Element c) -> c -> Element c
	foldr1 f c = fromMaybe (error "foldr1: empty structure") (foldr mf Nothing c) where
		mf x Nothing = Just x
		mf x (Just y) = Just (f x y)

	-- A variant of foldl that has no base case.
	-- May only be applied to non-empty Collections.
	foldl1 :: (Element c -> Element c -> Element c) -> c -> Element c
	foldl1 f c = fromMaybe (error "foldl1: empty structure") (foldl mf Nothing c) where
		mf Nothing y = Just y
		mf (Just x) y = Just (f x y)

