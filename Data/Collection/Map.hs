-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Map (
	Map(..)  ) where

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
	-- If the association already exists the original Association is returned.
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
	getValue :: DC.Element (Keys a) -> a -> Maybe (Value a)

-------------------------------------------------------------------------------

-- Performant way of making combined queries.
class Map a => MapCombine a where

	-- Get the associated value of the provided key before removing it.
	getValueAndRemoveKey :: DC.Element (Keys a) -> a -> (Maybe (Value a), a)

