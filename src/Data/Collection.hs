-- Author: Federico Mastellone (fmaste@gmail.com)

-- Collection classes.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection (
	Collection(..),
	Foldable(..),
	Foldable1(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Prelude hiding (foldr, foldl)
import Data.Maybe (fromMaybe)

-- CLASSES
-------------------------------------------------------------------------------

-- The main Collection class.
class Collection c where
	-- The Collection element type family.
	type Element c

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- Adds an Element to the Collection.
	-- If the Element already exists the result is implementation dependant.
	addElement :: Element c -> c -> c

	-- Removes an Element from the Collection.
	-- If the Element does not exists the original Collection is returned.
	removeElement :: Element c -> c -> c

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	-- True if the Collection contains this Element, otherwise, false.
	containsElement :: Element c -> c -> Bool

	-- The number of Elements that the Collection contains.
	getElementsCount :: c -> Integer

-------------------------------------------------------------------------------

-- Foldable class for Collections.
class Collection c => Foldable c where

	-- Right-associative fold of a Collection.
	foldr :: (Element c -> a -> a) -> a -> c -> a

	-- Left-associative fold of a Collection.
	foldl :: (a -> Element c -> a) -> a -> c -> a

	-- Fold over the elements of a Collection, associating to the right, but strictly.
	foldr' :: (Element c -> a -> a) -> a -> c -> a
	foldr' = defaultFoldr'

	-- Fold over the elements of a Collection, associating to the left, but strictly.
	foldl' :: (a -> Element c -> a) -> a -> c -> a
	foldl' = defaultFoldl'

defaultFoldr' :: (Collection c, Foldable c) => (Element c -> a -> a) -> a -> c -> a
defaultFoldr' f z0 xs = foldl f' id xs z0 where 
	f' g x z = g $! f x z

defaultFoldl' :: (Collection c, Foldable c) => (a -> Element c -> a) -> a -> c -> a
defaultFoldl' f z0 xs = foldr f' id xs z0 where 
	f' x g z = g $! f z x

-------------------------------------------------------------------------------

-- Collections like Map and MapSet can't implement this functions, so they are separated from Foldable.
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

