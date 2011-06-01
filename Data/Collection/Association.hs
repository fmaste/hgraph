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

	-- Add a key without any associated value to the Association.
	-- If the key already exists the original Association is returned.
	addKey :: Key a -> a -> a

	-- Remove a key with its associated values from the Association.
	-- If the key does not exists the original Association is returned.
	removeKey :: Key a -> a -> a

	-- Adds a (key, value) association to the Association.
	-- If the association already exists the original Association is returned.
	addAssociation :: Key a -> Value a -> a -> a

	-- Removes a (key, value) association from the Association.
	-- If the association does not exists the original Association is returned.
	removeAssociation :: Key a -> Value a -> a -> a

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	-- The number of (key, alue) associaitons that the Association contains.
	getAssociationsCount :: a -> Integer

	-- True if the Association contains this (key, value) association, otherwise, false.
	containsAssociation :: Key a -> Value a -> a -> Bool

	-- A list with the (key, value) associations that are members of the Association.
	getAssociationsList :: a -> [(key a, Value a)]

