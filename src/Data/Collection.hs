-- Author: Federico Mastellone (fmaste@gmail.com)

-- Collection classes.
-- TODO: Make it haddock compatible!

-- A container of zero or more Elements were the order is not important!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection (
	Collection(..) ) where

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

