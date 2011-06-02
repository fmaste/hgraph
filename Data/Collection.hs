-- Author: Federico Mastellone (fmaste@gmail.com)

-- Collection class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection (
	Collection(..)  ) where

-- CLASS
-------------------------------------------------------------------------------

class Collection c where
	-- The collection element type family.
	type Element c

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- The empty Collection.
	empty :: c

	-- Adds an Element to the Collection.
	-- If the Element already exists the original Collection is returned.
	addElement :: Element c -> c -> c

	-- Removes an Element from the Collection.
	-- If the Element does not exists the original Collection is returned.
	removeElement :: Element c -> c -> c

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	-- The number of Elements that the Collection contains.
	getElementsCount :: c -> Integer

	-- True if the Collection contains this Element, otherwise, false.
	containsElement :: Element c -> c -> Bool

	-- A list with the Elements that are members of the Collection.
	getElementsList :: c -> [Element c]

