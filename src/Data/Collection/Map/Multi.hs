-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association of the type (Key, Collection Value) class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Map.Multi (
	DC.Collection(..),
	DCM.Map(..),
	DCM.Combination(..),
	MultiMap(..),
	Batch(..),
	Foldable(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Prelude hiding (foldr, foldl)
import Data.Maybe (fromMaybe)
import qualified Data.Collection as DC
import qualified Data.Collection.Map as DCM

-- CLASSES
-------------------------------------------------------------------------------

class (DCM.Map a, DC.Collection (DCM.Value a)) => MultiMap a where

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- Adds a key and associate it to an empty collection.
	-- If the key already exists the original MultiMap is returned.
	addKey :: DC.Element (DCM.Keys a) -> a -> a

	-- Adds a collection value to key.
	-- If key does not exist it is added.
	addToKey :: DC.Element (DCM.Keys a) -> DC.Element (DCM.Value a) -> a -> a

	-- Removes the collection value from the key.
	-- If key does not exist the original MultiMap is returned.
	-- If the collection value does not exists the original MultiMap is returned.
	removeFromKey :: DC.Element (DCM.Keys a) -> DC.Element (DCM.Value a) -> a -> a

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------
	
	-- Collection value exists for the key?
	containedInKey :: DC.Element (DCM.Keys a) -> DC.Element (DCM.Value a) -> a -> Bool

	-- The number of collection values that the key has.
	getValuesCount :: DC.Element (DCM.Keys a) -> a -> Integer

-------------------------------------------------------------------------------

-- Performant functions to operate on more than one element.
-- No default implementations because they must be performant.
-- Use fold if your MultiMap if not an instance and you need this functions.
class MultiMap a => Batch a where

	-- Remove the value from all the keys.
	remove :: DC.Element (DCM.Value a) -> a -> a

	-- Remove the value from the provided keys.
	removeFromKeys :: [DC.Element (DCM.Keys a)] -> DC.Element (DCM.Value a) -> a -> a

-------------------------------------------------------------------------------

-- Foldable class for Map.
class MultiMap m => Foldable m where

	-- Right-associative fold of a Map.
	foldr :: (DC.Element (DCM.Value m) -> a -> a) -> a -> m -> a

	-- Left-associative fold of a Map.
	foldl :: (a -> DC.Element (DCM.Value m) -> a) -> a -> m -> a

	-- Fold over the elements of a Map, associating to the right, but strictly.
	foldr' :: (DC.Element (DCM.Value m) -> a -> a) -> a -> m -> a
	foldr' = defaultFoldr'

	-- Fold over the elements of a Map, associating to the left, but strictly.
	foldl' :: (a -> DC.Element (DCM.Value m) -> a) -> a -> m -> a
	foldl' = defaultFoldl'

	-- Right-associative fold with key of a Map.
	foldrWithKey :: (DC.Element (DCM.Keys m) -> DC.Element (DCM.Value m) -> a -> a) -> a -> m -> a

	-- Left-associative fold with key of a Map.
	foldlWithKey :: (a -> DC.Element (DCM.Keys m) -> DC.Element (DCM.Value m) -> a) -> a -> m -> a

	-- Fold over the elements of a Map with key, associating to the right, but strictly.
	foldrWithKey' :: (DC.Element (DCM.Keys m) -> DC.Element (DCM.Value m) -> a -> a) -> a -> m -> a
	foldrWithKey' = defaultFoldrWithKey'

	-- Fold over the elements of a Map with key, associating to the left, but strictly.
	foldlWithKey' :: (a -> DC.Element (DCM.Keys m) -> DC.Element (DCM.Value m) -> a) -> a -> m -> a
	foldlWithKey' = defaultFoldlWithKey'

defaultFoldr' :: (MultiMap m, Foldable m) => (DC.Element (DCM.Value m) -> a -> a) -> a -> m -> a
defaultFoldr' f z0 xs = foldl f' id xs z0 where 
	f' g x z = g $! f x z

defaultFoldl' :: (MultiMap m, Foldable m) => (a -> DC.Element (DCM.Value m) -> a) -> a -> m -> a
defaultFoldl' f z0 xs = foldr f' id xs z0 where 
	f' x g z = g $! f z x

defaultFoldrWithKey' :: (MultiMap m, Foldable m) => (DC.Element (DCM.Keys m) -> DC.Element (DCM.Value m) -> a -> a) -> a -> m -> a
defaultFoldrWithKey' f z0 xs = foldlWithKey f' id xs z0 where 
	f' g k x z = g $! f k x z

defaultFoldlWithKey' :: (MultiMap m, Foldable m) => (a -> DC.Element (DCM.Keys m) -> DC.Element (DCM.Value m) -> a) -> a -> m -> a
defaultFoldlWithKey' f z0 xs = foldrWithKey f' id xs z0 where 
	f' k x g z = g $! f z k x

