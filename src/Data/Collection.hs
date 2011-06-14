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

