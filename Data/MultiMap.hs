-- Author: Federico Mastellone (fmaste@gmail.com)

-- Every key has a set of elements.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.MultiMap (
	-- Atomic construction functions.
	MultiMap(),
	empty,
	addKey,
	removeKey,
	addValue,
	removeValue,
	-- Atomic query functions.
	getKeys,
	getValues,
	-- Util functions.
	isEmpty,
	getKeysSet,
	getKeyCount,
	getValuesList,
	getValueCount,
	containsKey,
	containsValue,
	getValuesAndRemoveKey,
	removeValueFromKeys,
	removeValuesAll,
	-- SOMETHING
	mapSet,
	foldSet,
	foldSetWithKey) where

-- IMPORTS
-------------------------------------------------------------------------------

import Data.List (foldl, foldl', foldr)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Collection as DC

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
-- If the value already exists the orignal MultiMap is returned.
addValue :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
addValue k v (MultiMap m) = MultiMap $ Map.insertWith (\new old -> Set.insert v old) k (Set.singleton v) m

-- | Removes the value from the key.
-- If key does not exist the original MultiMap is returned.
-- If the value does not exists the original MultiMap is returned.
removeValue :: (Ord k, Ord v) => k -> v ->  MultiMap k v ->  MultiMap k v
removeValue k v (MultiMap m) = MultiMap $ Map.adjust (Set.delete v) k m

-- * ATOMIC QUERY FUNCTIONS
-------------------------------------------------------------------------------

-- | A list with all the different keys.
getKeys :: (Ord k, Ord v) => MultiMap k v -> [k]
getKeys (MultiMap m) = Map.keys m

-- | A set with the different values that exist for the key.
-- If key does not exist an empty Set is returned.
getValues :: (Ord k, Ord v) => k -> MultiMap k v -> Set.Set v
getValues k (MultiMap m) = Map.findWithDefault Set.empty k m

-- * UTILS FUNCTIONS
-------------------------------------------------------------------------------

isEmpty :: (Ord k, Ord v) => MultiMap k v -> Bool
isEmpty (MultiMap m) = Map.null m

-- | A set with all the different keys.
getKeysSet :: (Ord k, Ord v) => MultiMap k v -> Set.Set k
getKeysSet (MultiMap m) = Map.keysSet m

-- | The number of different keys present.
getKeyCount :: (Ord k, Ord v) => MultiMap k v -> Int
getKeyCount (MultiMap m) = Map.size m

-- | All the different values that exist for the key.
getValuesList :: (Ord k, Ord v) => k -> MultiMap k v -> [v]
getValuesList k mm = Set.elems $ getValues k mm

-- | The number of different values that exist for the key.
getValueCount :: (Ord k, Ord v) => k -> MultiMap k v -> Int
getValueCount k mm = Set.size $ getValues k mm

-- | Key exists?
containsKey :: (Ord k, Ord v) => k -> MultiMap k v -> Bool
containsKey k (MultiMap m) = Map.member k m

-- | Value exists for the key?
containsValue :: (Ord k, Ord v) => k -> v -> MultiMap k v -> Bool
containsValue k v mm = Set.member v $ getValues k mm

getValuesAndRemoveKey :: (Ord k, Ord v) => k -> MultiMap k v -> (MultiMap k v, [v])
getValuesAndRemoveKey k (MultiMap m) = f $ Map.updateLookupWithKey (\_ _ -> Nothing) k m where
	f (Nothing, m) = (MultiMap m, [])
	f (Just v, m) = (MultiMap m, Set.elems v)

removeValueFromKeys :: (Ord k, Ord v) => [k] -> v -> MultiMap k v -> MultiMap k v
removeValueFromKeys ks v (MultiMap m) = MultiMap (Map.unionWith f m m') where
	f set _ = Set.delete v set
	m' = Map.fromList $ map g ks where
		g k = (k, Set.empty)

-- | Removes all the values from the key, the key is retained with no values.
-- If key does not exist the original MultiMap is returned.
-- If there are no values the original MultiMap is returned.
removeValuesAll :: (Ord k, Ord v) => k -> MultiMap k v ->  MultiMap k v
removeValuesAll k (MultiMap m) = MultiMap $ Map.adjust (const Set.empty) k m

mapSet :: (Ord k, Ord v, Ord v') => (Set.Set v -> Set.Set v') -> MultiMap k v -> MultiMap k v'
mapSet f (MultiMap mm) = MultiMap (Map.map f mm)

foldSet :: (Ord k, Ord v) => (Set.Set v -> ans -> ans) -> ans -> MultiMap k v -> ans
foldSet f ans (MultiMap mm) = Map.fold f ans mm

foldSetWithKey :: (Ord k, Ord v) => (k -> Set.Set v -> ans -> ans) -> ans -> MultiMap k v -> ans
foldSetWithKey f ans (MultiMap mm) = Map.foldWithKey f ans mm

-- * QUERY FUNCTIONS
-------------------------------------------------------------------------------

-- TODO

-- INSTANCE
-------------------------------------------------------------------------------

instance (Ord k, Ord v) => DC.Collection (MultiMap k v) where
	type DC.Element (MultiMap k v) = (k, v)
	empty = empty
	addElement (k, v) = addValue k v
	removeElement (k, v) = removeValue k v
	containsElement (k, v) m = Set.member v $ getValues k m
	getElementsCount m = toInteger $ foldSet (\set ans -> ans + (Set.size set)) 0 m
	getElementsList m = foldSetWithKey (\k set ans -> ans ++ [(k, v) | v <- (Set.elems set)]) [] m

