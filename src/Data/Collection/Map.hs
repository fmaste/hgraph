-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Map (
	DC.Collection(..),
	Map(..),
	Combination(..),
	Foldable(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Prelude hiding (foldr, foldl)
import Data.Maybe (fromMaybe)
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
	putValue :: DC.Element (Keys m) -> Value m -> m -> m

	-- Remove a key with its associated value from the Association.
	-- If the key does not exists the original Association is returned.
	removeKey :: DC.Element (Keys m) -> m -> m

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	-- All the keys.
	getKeys :: m -> Keys m

	-- True if the Association contains this Key, otherwise, false.
	containsKey :: DC.Element (Keys m) -> m -> Bool

	-- The number of keys that the Map has.
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

-------------------------------------------------------------------------------

-- Foldable class for Map.
class Map m => Foldable m where

	-- Right-associative fold of a Map.
	foldr :: (Value m -> a -> a) -> a -> m -> a

	-- Left-associative fold of a Map.
	foldl :: (a -> Value m -> a) -> a -> m -> a

	-- Fold over the elements of a Map, associating to the right, but strictly.
	foldr' :: (Value m -> a -> a) -> a -> m -> a
	foldr' = defaultFoldr'

	-- Fold over the elements of a Map, associating to the left, but strictly.
	foldl' :: (a -> Value m -> a) -> a -> m -> a
	foldl' = defaultFoldl'

	-- Right-associative fold with key of a Map.
	foldrWithKey :: (DC.Element (Keys m) -> Value m -> a -> a) -> a -> m -> a

	-- Left-associative fold with key of a Map.
	foldlWithKey :: (a -> DC.Element (Keys m) -> Value m -> a) -> a -> m -> a

	-- Fold over the elements of a Map with key, associating to the right, but strictly.
	foldrWithKey' :: (DC.Element (Keys m) -> Value m -> a -> a) -> a -> m -> a
	foldrWithKey' = defaultFoldrWithKey'

	-- Fold over the elements of a Map with key, associating to the left, but strictly.
	foldlWithKey' :: (a -> DC.Element (Keys m) -> Value m -> a) -> a -> m -> a
	foldlWithKey' = defaultFoldlWithKey'

defaultFoldr' :: (Map m, Foldable m) => (Value m -> a -> a) -> a -> m -> a
defaultFoldr' f z0 xs = foldl f' id xs z0 where 
	f' g x z = g $! f x z

defaultFoldl' :: (Map m, Foldable m) => (a -> Value m -> a) -> a -> m -> a
defaultFoldl' f z0 xs = foldr f' id xs z0 where 
	f' x g z = g $! f z x

defaultFoldrWithKey' :: (Map m, Foldable m) => (DC.Element (Keys m) -> Value m -> a -> a) -> a -> m -> a
defaultFoldrWithKey' f z0 xs = foldlWithKey f' id xs z0 where 
	f' g k x z = g $! f k x z

defaultFoldlWithKey' :: (Map m, Foldable m) => (a -> DC.Element (Keys m) -> Value m -> a) -> a -> m -> a
defaultFoldlWithKey' f z0 xs = foldrWithKey f' id xs z0 where 
	f' k x g z = g $! f z k x

