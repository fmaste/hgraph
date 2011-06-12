-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Map (
	DC.Collection(..),
	Map(..),
	Combination(..)  ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC

-- CLASSES
-------------------------------------------------------------------------------

-- The main Map class.
class (DC.Collection a, DC.Collection (Keys a)) => Map a where
	-- The Assocation type families.
	type Keys a
	type Value a

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- Associates a value with the provided key.
	putValue :: DC.Element (Keys a) -> Value a -> a -> a

	-- Remove a key with its associated value from the Association.
	-- If the key does not exists the original Association is returned.
	removeKey :: DC.Element (Keys a) -> a -> a

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	-- All the keys.
	getKeys :: a -> Keys a

	-- True if the Association contains this Key, otherwise, false.
	containsKey :: DC.Element (Keys a) -> a -> Bool

	-- The number of keys that the Map has.
	getKeysCount :: a -> Integer

	-- Get the associated value of the provided key.
	-- If the key does not exists error is called.
	getValue :: DC.Element (Keys a) -> a -> Value a

-------------------------------------------------------------------------------

-- Performant way of making a combination of the above Map atomic functions.
class Map a => Combination a where

	-- Get, maybe, the associated value of the provided key.
	getValueMaybe :: DC.Element (Keys a) -> a -> Maybe (Value a)

	-- If there is no value fot this key, the provided default is returned.
	getValueWithDefault :: Value a -> DC.Element (Keys a) -> a -> Value a

	-- Get the associated value of the provided key before removing it.
	getValueAndRemoveKey :: DC.Element (Keys a) -> a -> (Maybe (Value a), a)
