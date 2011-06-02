-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Association (
	Association(..)  ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC

-- CLASS
-------------------------------------------------------------------------------

class DC.Collection a => Association a where
	-- The Assocation type families.
	type Key a
	type Value a

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- Adds a (key, value) association to the Association.
	-- If the association already exists the original Association is returned.
	addValue :: Key a -> Value a -> a -> a

	-- Remove a key with its associated value from the Association.
	-- If the key does not exists the original Association is returned.
	removeKey :: Key a -> a -> a

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	-- True if the Association contains this Key, otherwise, false.
	containsKey :: Key a -> a -> Bool

	-- Get the associated value of the provided key.
	getValue :: Key a -> a -> Maybe (Value a)

