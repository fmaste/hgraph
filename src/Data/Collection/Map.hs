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
class (DC.Collection m, DC.Collection (Keys m)) => Map m where
	-- The Assocation type families.
	type Keys m
	type Value m

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- Associates a value with the provided key.
	-- If the key already has an associated value it is replaced.
	putValue :: DC.Element (Keys m) -> Value m -> m -> m

	-- Remove a key with its associated value from the Association.
	-- If the key does not exists the original Association is returned.
	removeKey :: DC.Element (Keys m) -> m -> m

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	-- Get the associated value of the provided key.
	-- If the key does not exists Nothing is returned.
	getValue :: DC.Element (Keys m) -> m -> Maybe (Value m)

-------------------------------------------------------------------------------

class Map m => MapKeys m where

	-- All the keys that have an associated value.
	getKeys :: m -> Keys m

	-- True if the key has an associated value, otherwise, false.
	containsKey :: DC.Element (Keys m) -> m -> Bool

	-- The number of keys that have an associated value.
	getKeysCount :: m -> Integer

-------------------------------------------------------------------------------

-- Performant way of making a combination of the above Map atomic functions.
class Map m => Combination m where

	-- If there is no value fot this key, the provided default is returned.
	getValueWithDefault :: Value m -> DC.Element (Keys m) -> m -> Value m

	-- Get the associated value of the provided key before removing it.
	getValueAndRemoveKey :: DC.Element (Keys m) -> m -> (Maybe (Value m), m)

