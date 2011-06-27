-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Map (
	DC.Collection(..),
	Map(..),
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

	-- All the keys that have an associated value.
	getKeys :: m -> Keys m

	-- True if the Association contains this Key, otherwise, false.
	containsKey :: DC.Element (Keys m) -> m -> Bool

	-- The number of keys that have an associated value.
	getKeysCount :: m -> Integer

	-- Get the associated value of the provided key.
	-- If the key does not exists error is called.
	getValue :: DC.Element (Keys m) -> m -> Value m

-------------------------------------------------------------------------------

-- Performant way of making a combination of the above Map atomic functions.
class Map m => Combination m where

	-- Get, maybe, the associated value of the provided key.
	getValueMaybe :: DC.Element (Keys m) -> m -> Maybe (Value m)

	-- If there is no value fot this key, the provided default is returned.
	getValueWithDefault :: Value m -> DC.Element (Keys m) -> m -> Value m

	-- Get the associated value of the provided key before removing it.
	getValueAndRemoveKey :: DC.Element (Keys m) -> m -> (Maybe (Value m), m)

