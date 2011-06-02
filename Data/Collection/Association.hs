-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Association (
	Association(..)  ) where

-- CLASS
-------------------------------------------------------------------------------

class Association a where
	-- The Assocation type families.
	type Key a
	type Value a

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- The empty Association.
	empty :: a

	-- Adds a (key, value) association to the Association.
	-- If the association already exists the original Association is returned.
	addAssociation :: Key a -> Value a -> a -> a

	-- Remove a key with its associated value from the Association.
	-- If the key does not exists the original Association is returned.
	removeKey :: Key a -> a -> a

	-- Removes a (key, value) association from the Association.
	-- If the association does not exists the original Association is returned.
	-- TODO: removeAssociation :: Key a -> Value a -> a -> a

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	-- True if the Association contains this Key, otherwise, false.
	containsKey :: Key a -> a -> Bool

	-- Get the asociated value of the provided key.
	getAssociatedValue :: Key a -> a -> Maybe (Value a)

	-- The number of (key, alue) associaitons that the Association contains.
	getAssociationsCount :: a -> Integer

	-- A list with the (key, value) associations that are members of the Association.
	getAssociationsList :: a -> [(key a, Value a)]

