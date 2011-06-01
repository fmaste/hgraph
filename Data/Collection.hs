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

	-- Adds an element to the Collection.
	-- If the element already exists the original Collection is returned.
	insert :: Element c -> c -> c

	-- Removes an element from the Collection.
	-- If the element does not exists the original Collection is returned.
	delete :: Element c -> c -> c

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	-- The number of elements that the Collection contains.
	size :: c -> Integer

	-- True if the Collection contains this element, otherwise, false.
	member :: Element c -> c -> Bool

	-- A list with the elements that are members of the Collection.
	elems :: c -> [Element c]

