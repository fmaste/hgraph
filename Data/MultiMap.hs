-- Author: Federico Mastellone (fmaste@gmail.com)

module Data.MultiMap (
	MultiMap(),
	empty,
	addKey,
	removeKey,
	addValue,
	removeValue,
	isEmpty,
	getKeys,
	getKeysSet,
	getKeyCount,
	getValues,
	getValuesSet,
	getValueCount,
	containsKey,
	containsValue,
	removeValuesAll) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Data.Set as Set

-- * DATA DEFINITION
-------------------------------------------------------------------------------

newtype MultiMap k v = MultiMap (Map.Map k (Set.Set v))
	deriving (Show, Read, Ord, Eq)

-- TODO: Make a generic result type, so it is not dependent of the implementation.
-- Also provide getList and getSet functions but optimized for every implementation.
-- import qualified Control.Monad as Monad
-- import qualified Data.Foldable as Foldable
-- import qualified Data.Traversable as Traversable
--newtype ResultType a = Result [a]
--	deriving (Monad.Functor, Foldable.Foldable, Traversable.Traversable)

-- * ATOMIC CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- | The empty MultiMap.
empty :: (Ord k, Ord v) => MultiMap k v
empty = MultiMap $ Map.empty

-- | Adds a key without any values.
-- If the key already exists the original MultiMap is returned.
addKey :: (Ord k, Ord v) => k -> MultiMap k v -> MultiMap k v
addKey k (MultiMap m) = MultiMap $ Map.union m (Map.singleton k Set.empty)

-- | Removes the key and all its values.
-- If the key does not exists the original MultiMap is returned.
removeKey :: (Ord k, Ord v) => k -> MultiMap k v -> MultiMap k v
removeKey k (MultiMap m) = MultiMap $ Map.delete k m

-- | Adds a value to key.
-- If key does not exist it is added.
-- If the value already exists the same MultiMap is returned.
addValue :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
addValue k v (MultiMap m) = MultiMap $ Map.insertWith (\new old -> Set.insert v old) k (Set.singleton v) m

-- | Removes the value from the key.
-- If key does not exist the original MultiMap is returned.
-- If the value does not exists the original MultiMap is returned.
removeValue :: (Ord k, Ord v) => k -> v ->  MultiMap k v ->  MultiMap k v
removeValue k v (MultiMap m) = MultiMap $ Map.adjust (Set.delete v) k m

-- * ATOMIC QUERY FUNCTIONS
-------------------------------------------------------------------------------

isEmpty :: (Ord k, Ord v) => MultiMap k v -> Bool
isEmpty (MultiMap m) = Map.null m

-- | A list with all the different keys.
getKeys :: (Ord k, Ord v) => MultiMap k v -> [k]
getKeys (MultiMap m) = Map.keys m

-- | A set with all the different keys.
getKeysSet :: (Ord k, Ord v) => MultiMap k v -> Set.Set k
getKeysSet (MultiMap m) = Map.keysSet m

-- | The number of different keys present.
getKeyCount :: (Ord k, Ord v) => MultiMap k v -> Int
getKeyCount (MultiMap m) = Map.size m

-- | All the different values that exist for the key.
getValues :: (Ord k, Ord v) => k -> MultiMap k v -> [v]
getValues k mm = Set.elems $ getValuesSet k mm

-- | A set with the different values that exist for the key.
-- If key does not exist an empty Set is returned.
getValuesSet :: (Ord k, Ord v) => k -> MultiMap k v -> Set.Set v
getValuesSet k (MultiMap m) = Map.findWithDefault Set.empty k m

-- | The number of different values that exist for the key.
getValueCount :: (Ord k, Ord v) => k -> MultiMap k v -> Int
getValueCount k mm = Set.size $ getValuesSet k mm

-- | Key exists?
containsKey :: (Ord k, Ord v) => k -> MultiMap k v -> Bool
containsKey k (MultiMap m) = Map.member k m

-- | Value exists for the key?
containsValue :: (Ord k, Ord v) => k -> v -> MultiMap k v -> Bool
containsValue k v mm = Set.member v $ getValuesSet k mm

-- * CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- | Removes all the values from the key, the key is retained with no values.
-- If key does not exist the original MultiMap is returned.
-- If there are no values the original MultiMap is returned.
removeValuesAll :: (Ord k, Ord v) => k -> MultiMap k v ->  MultiMap k v
removeValuesAll k (MultiMap m) = MultiMap $ Map.adjust (const Set.empty) k m

-- * QUERY FUNCTIONS
-------------------------------------------------------------------------------

-- TODO
