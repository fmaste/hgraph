-- Author: Federico Mastellone (fmaste@gmail.com)

-- Collection classes.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection (
	Collection(..),
	CollectionList(..),
	CollectionBatch(..) ) where

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


-- List conversion functions.
class Collection c => CollectionList c where

	-- Export to a list representation.
	toList :: c -> [Element c]

	-- Import from a list representation.
	fromList :: [Element c] -> c


-- Functions to operate on more than one element.
class Collection c => CollectionBatch c where
	-- TODO: Provide default implementations	
	addElements :: [Element c] -> c -> c
	removeElements :: [Element c] -> c -> c
	containsElements :: [Element c] -> c -> Bool

