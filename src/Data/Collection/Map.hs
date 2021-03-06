-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association class.
-- TODO: Make it haddock compatible!

-- An efficient way to make (key, value) associations.
-- You can think about it as a binary relation that is a function. 
-- Where, the domain is a set of Keys and the codomain is a bag of Values.
-- Every domain element is always associated with only one value on the codomain.
-- Every codomain element is always associated with only one key on the domain.

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Map (
	Map(..),
	Combination(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC

-- CLASSES
-------------------------------------------------------------------------------

-- The main Map class.
class DC.Collection m => Map m where
	-- The Assocation type families.
	type Key m
	type Value m

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- Associates a value with the provided key.
	-- If the key already has an associated value it is replaced.
	putValue :: Key m -> Value m -> m -> m

	-- Remove a key with its associated value from the Association.
	-- If the key does not exists the original Association is returned.
	removeKey :: Key m -> m -> m

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	-- All the keys that have an associated value.
	-- TODO: Create type KeysSet with class context (Set (KeysSet m) , (Element (KeysSet m)) ~ Key m)
	getKeys :: m -> [Key m]

	-- Get the associated value of the provided key.
	-- If the key does not exists Nothing is returned.
	getValue :: Key m -> m -> Maybe (Value m)

-------------------------------------------------------------------------------

-- Performant way of making a combination of the above Map atomic functions.
class Map m => Combination m where

	-- If there is no value fot this key, the provided default is returned.
	getValueWithDefault :: Value m -> Key m -> m -> Value m

	-- Get the associated value of the provided key before removing it.
	getValueAndRemoveKey :: Key m -> m -> (Maybe (Value m), m)

