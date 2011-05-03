-- Author: Federico Mastellone (fmaste@gmail.com)

module Data.MultiMap (
	MultiMap(),
	empty,
	addKey,
	removeKey,
	addValue,
	removeValue,
	removeAllValues,
	isEmpty,
	getKeys,
	getKeyCount,
	getValues,
	getValuesSet,
	getValueCount,
	containsKey,
	containsValue) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Data.Set as Set

-- * DATA DEFINITION
-------------------------------------------------------------------------------

type MultiMap k v = Map.Map k (Set.Set v)

-- * CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- | The empty MultiMap.
empty :: (Ord k, Ord v) => MultiMap k v
empty = Map.empty

-- | Adds a key without any values.
-- If the key already exists the original MultiMap is returned.
addKey :: (Ord k, Ord v) => k -> MultiMap k v -> MultiMap k v
addKey k m = Map.insertWith (\new old -> old) k Set.empty m

-- | Removes the key and all its values.
-- If the key does not exists the original MultiMap is returned.
removeKey :: (Ord k, Ord v) => k -> MultiMap k v -> MultiMap k v
removeKey k m = Map.delete k m

-- | Adds an value to key.
-- If key does not exist it is added.
-- If the value already exists the original MultiMap is returned.
addValue :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
addValue k v m = Map.insertWith (\new old -> Set.insert v old) k (Set.singleton v) m

-- | Removes the value from the key.
-- If key does not exist the original MultiMap is returned.
-- If the value does not exists the original MultiMap is returned.
removeValue :: (Ord k, Ord v) => k -> v ->  MultiMap k v ->  MultiMap k v
removeValue k v m = Map.adjust (Set.delete v) k m

-- | Removes all the values from the key and the key is retained with no values.
-- If key does not exist the original MultiMap is returned.
-- If there are no values the original MultiMap is returned.
removeAllValues :: (Ord k, Ord v) => k -> MultiMap k v ->  MultiMap k v
removeAllValues k m = Map.adjust (const Set.empty) k m

-- * QUERY FUNCTIONS
-------------------------------------------------------------------------------

isEmpty :: (Ord k, Ord v) => MultiMap k v -> Bool
isEmpty m = Map.null m

-- | A list with all the different keys.
getKeys :: (Ord k, Ord v) => MultiMap k v -> [k]
getKeys m = Map.keys m

-- | The number of different keys present.
getKeyCount :: (Ord k, Ord v) => MultiMap k v -> Int
getKeyCount m = Map.size m

-- | All the different values that exist for the key.
getValues :: (Ord k, Ord v) => k -> MultiMap k v -> [v]
getValues k m = Set.elems $ getValuesSet k m

-- | A set with the different values that exist for the key.
-- If key does not exist an empty Set is returned.
getValuesSet :: (Ord k, Ord v) => k -> MultiMap k v -> Set.Set v
getValuesSet k m = Map.findWithDefault Set.empty k m

-- | The number of different values that exist for the key.
getValueCount :: (Ord k, Ord v) => k -> MultiMap k v -> Int
getValueCount k m = Set.size $ getValuesSet k m

-- | Key exists?
containsKey :: (Ord k, Ord v) => k -> MultiMap k v -> Bool
containsKey k m = Map.member k m

-- | Value exists?
containsValue :: (Ord k, Ord v) => k -> v -> MultiMap k v -> Bool
containsValue k v m = Set.member v $ getValuesSet k m
