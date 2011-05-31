-- Author: Federico Mastellone (fmaste@gmail.com)

-- Set class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Set (
	Set,
	empty,
	insert,
	delete,
	size,
	member,
	elems ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Set as DS

-- CLASS
-------------------------------------------------------------------------------

class Set s where
	-- The set element type family.
	type Element s

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- The empty Set.
	empty :: s

	-- Adds an element to the Set.
	-- If the element already exists the original Set is returned.
	insert :: Element s -> s -> s

	-- Removes an element from the Set.
	-- If the element does not exists the original Set is returned.
	delete :: Element s -> s -> s

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	-- The number of elements that the Set contains.
	size :: s -> Integer

	-- True if the Set contains this element, otherwise, false.
	member :: Element s -> s -> Bool

	-- A list with the elements that are members of the Set.
	elems :: s -> [Element s]

-- INSTANCE
-------------------------------------------------------------------------------

instance Ord a => Set (DS.Set a) where
	type Element (DS.Set a) = a
	empty = DS.empty
	insert = DS.insert
	delete = DS.delete
	size = toInteger . DS.size
	member = DS.member
	elems = DS.elems

