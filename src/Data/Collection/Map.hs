-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Map (
	Map(..),
	MapKeys(..),
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

	-- Get the associated value of the provided key.
	-- If the key does not exists Nothing is returned.
	getValue :: Key m -> m -> Maybe (Value m)

-------------------------------------------------------------------------------

class Map m => MapKeys m where

	-- All the keys that have an associated value.
	getKeys :: m -> [Key m]

	-- True if the key has an associated value, otherwise, false.
	containsKey :: Key m -> m -> Bool

	-- The number of keys that have an associated value.
	getKeysCount :: m -> Integer

-------------------------------------------------------------------------------

class Map m => MapValues m where

	-- All the values that are associated with a key.
	getValues :: m -> [Value m]

	-- True if the value is associated with any key, otherwise, false.
	containsValue :: Value m -> m -> Bool

	-- Remove the keys that have this associated value.
	removeValue :: Value m -> m -> m

-------------------------------------------------------------------------------

-- Performant way of making a combination of the above Map atomic functions.
class Map m => Combination m where

	-- If there is no value fot this key, the provided default is returned.
	getValueWithDefault :: Value m -> Key m -> m -> Value m

	-- Get the associated value of the provided key before removing it.
	getValueAndRemoveKey :: Key m -> m -> (Maybe (Value m), m)

