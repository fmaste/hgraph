-- Author: Federico Mastellone (fmaste@gmail.com)

-- Collection classes.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection (
	Collection(..),
	List(..),
	Batch(..),
	Foldable(..) ) where

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
	-- If the Element already exists the original Collection is returned.
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

-- List conversion functions.
class Collection c => List c where

	-- Export to a list representation.
	toList :: c -> [Element c]

	-- Import from a list representation.
	fromList :: [Element c] -> c

-------------------------------------------------------------------------------

-- Performant functions to operate on more than one element.
-- No default implementations because they must be performant.
-- Use fold if your Collection if not an instance and you need this functions.
class Collection c => Batch c where
	addElements :: [Element c] -> c -> c
	removeElements :: [Element c] -> c -> c
	containsElements :: [Element c] -> c -> Bool

-------------------------------------------------------------------------------

-- Foldable class for Collections.
class Collection c => Foldable c where

	-- Right-associative fold of a Collection.
	foldr :: (Element c -> a -> a) -> a -> c -> a

	-- Left-associative fold of a Collection.
	foldl :: (a -> Element c -> a) -> a -> c -> a

	-- Fold over the elements of a Collection, associating to the right, but strictly.
	foldr' :: (Element c -> a -> a) -> a -> c -> a
	foldr' f z0 xs = foldl f' id xs z0 where 
		f' k x z = k $! f x z

	-- Fold over the elements of a Collection, associating to the left, but strictly.
	foldl' :: (a -> Element c -> a) -> a -> c -> a
	foldl' f z0 xs = foldr f' id xs z0 where 
		f' x k z = k $! f z x

