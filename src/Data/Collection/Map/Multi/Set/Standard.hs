-- Author: Federico Mastellone (fmaste@gmail.com)

-- Every key has a set of elements.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Map.Multi.Set.Standard (
	MapSet(),
	empty,
	addElement,
	removeElement,
	containsElement,
	getElementsCount,
	toList,
	fromList,
	foldrElements,
	foldlElements,
	foldrElements',
	foldlElements',
	putValue,
	removeKey,
	getKeys,
	containsKey,
	getKeysCount,
	getValue,
	getValueMaybe,
	getValueWithDefault,
	getValueAndRemoveKey,
	foldrSet,
	foldlSet,
	foldrSet',
	foldlSet',
	foldrSetWithKey,
	foldlSetWithKey,
	foldrSetWithKey',
	foldlSetWithKey' ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Data.List (foldl, foldl', foldr)
import qualified Data.Map as Map -- TODO: Use Data.Collection.Map.Standard instead.
import qualified Data.Collection as DC
import qualified Data.Collection.List as DCL
import qualified Data.Collection.Foldable as DCF
import qualified Data.Collection.Map as DCM
import qualified Data.Collection.Map.Foldable as DCMF
import qualified Data.Collection.Map.Multi as DCMM
import qualified Data.Collection.Map.Multi.Set as DCMMS
import qualified Data.Collection.Set.Standard as Set

-- * DATA DEFINITION
-------------------------------------------------------------------------------

newtype MapSet k v = MapSet (Map.Map k (Set.Set v))
	deriving (Show, Read, Ord, Eq)

-- * ATOMIC CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- | The empty MapSet.
empty :: (Ord k, Ord v) => MapSet k v
empty = MapSet $ Map.empty

addElement :: (Ord k, Ord v) => (k, v) -> MapSet k v -> MapSet k v
addElement (k, v) = addValue k v

removeElement :: (Ord k, Ord v) => (k, v) -> MapSet k v -> MapSet k v
removeElement (k, v) = removeValue k v

containsElement :: (Ord k, Ord v) => (k, v) -> MapSet k v -> Bool
containsElement (k, v) m = Set.containsElement v $ getValues k m

getElementsCount :: (Ord k, Ord v) => MapSet k v -> Integer
getElementsCount m = toInteger $ foldSet (\set ans -> ans + (Set.getElementsCount set)) 0 m

toList :: (Ord k, Ord v) => MapSet k v -> [(k, v)]
toList m = foldSetWithKey (\k set ans -> ans ++ [(k, v) | v <- (DCL.toList set)]) [] m

fromList :: (Ord k, Ord v) => [(k, v)] -> MapSet k v
fromList list = foldl' (\ans (k, v) -> addValue k v ans) empty list

-- Collection version of fold

foldrElements :: (Ord k, Ord v) => ((k, v) -> a -> a) -> a -> MapSet k v -> a
foldrElements f a (MapSet m) = Map.foldrWithKey g a m where
	g k set a = DCF.foldr g' a set where
		g' v a = f (k, v) a

foldlElements :: (Ord k, Ord v) => (a -> (k, v) -> a) -> a -> MapSet k v -> a
foldlElements f a (MapSet m) = Map.foldlWithKey g a m where
	g a k set = DCF.foldl g' a set where
		g' a v = f a (k, v)

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldrElements' :: (Ord k, Ord v) => ((k, v) -> a -> a) -> a -> MapSet k v -> a
-- TODO: Make the same as foldrElements above but with strict folds.
foldrElements' f a m = DCF.foldr' f a m -- Use provided default implementation.

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldlElements' :: (Ord k, Ord v) => (a -> (k, v) -> a) -> a -> MapSet k v -> a
-- TODO: Make the same as foldlElements above but with strict folds.
foldlElements' f a m = DCF.foldl' f a m -- Use provided default implementation.

putValue :: (Ord k, Ord v) => k -> Set.Set v -> MapSet k v -> MapSet k v
putValue k vSet (MapSet m) = MapSet (Map.insert k vSet m)

-- | Removes the key and all its values.
-- If the key does not exists the original MapSet is returned.
removeKey :: (Ord k, Ord v) => k -> MapSet k v -> MapSet k v
removeKey k (MapSet m) = MapSet $ Map.delete k m

-- | A set with all the different keys.
getKeys :: (Ord k, Ord v) => MapSet k v -> Set.Set k
getKeys (MapSet m) = Map.keysSet m

-- | Key exists?
containsKey :: (Ord k, Ord v) => k -> MapSet k v -> Bool
containsKey k (MapSet m) = Map.member k m

getKeysCount :: MapSet k v -> Integer
getKeysCount (MapSet m) = toInteger $ Map.size m

getValue :: (Ord k, Ord v) => k -> MapSet k v -> Set.Set v
getValue k mm = getValues k mm

getValueMaybe :: (Ord k, Ord v) => k -> MapSet k v -> Maybe (Set.Set v)
getValueMaybe k mm = Just $ getValues k mm

getValueWithDefault :: (Ord k, Ord v) => Set.Set v -> k -> MapSet k v -> Set.Set v
getValueWithDefault _ k mm = getValues k mm

getValueAndRemoveKey :: (Ord k, Ord v) => k -> MapSet k v -> (Maybe (Set.Set v), MapSet k v)
getValueAndRemoveKey k mm = (Just $ getValues k mm, removeKey k mm)

foldrSet :: (Set.Set v -> a -> a) -> a -> MapSet k v -> a
foldrSet f a (MapSet m) = Map.foldrWithKey g a m where
	g k v a = f v a

foldlSet :: (a -> Set.Set v -> a) -> a -> MapSet k v -> a
foldlSet f a (MapSet m) = Map.foldlWithKey g a m where
	g a k v = f a v

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldrSet' :: (Ord k, Ord v) => (Set.Set v -> a -> a) -> a -> MapSet k v -> a
foldrSet' = DCMF.foldr' -- Use provided default implementation.

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldlSet' :: (Ord k, Ord v) => (a -> Set.Set v -> a) -> a -> MapSet k v -> a
foldlSet' = DCMF.foldl' -- Use provided default implementation.

foldrSetWithKey :: (k -> Set.Set v -> a -> a) -> a -> MapSet k v -> a
foldrSetWithKey f a (MapSet m) = Map.foldrWithKey f a m

foldlSetWithKey :: (a -> k -> Set.Set v -> a) -> a -> MapSet k v -> a
foldlSetWithKey f a (MapSet m) = Map.foldlWithKey f a m

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldrSetWithKey' :: (Ord k, Ord v) => (k -> Set.Set v -> a -> a) -> a -> MapSet k v -> a
foldrSetWithKey' = DCMF.foldrWithKey' -- Use provided default implementation.

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldlSetWithKey' :: (Ord k, Ord v) => (a -> k -> Set.Set v -> a) -> a -> MapSet k v -> a
foldlSetWithKey' = DCMF.foldlWithKey' -- Use provided default implementation.



-- | Adds a key without any values.
-- If the key already exists the original MapSet is returned.
addKey :: (Ord k, Ord v) => k -> MapSet k v -> MapSet k v
addKey k (MapSet m) = MapSet $ Map.union m (Map.singleton k Set.empty)

-- | Adds a value to key.
-- If key does not exist it is added.
-- If the value already exists the orignal MapSet is returned.
addValue :: (Ord k, Ord v) => k -> v -> MapSet k v -> MapSet k v
addValue k v (MapSet m) = MapSet $ Map.insertWith (\new old -> Set.addElement v old) k (singleton v) m
	where singleton v = Set.addElement v $ Set.empty

-- | Removes the value from the key.
-- If key does not exist the original MapSet is returned.
-- If the value does not exists the original MapSet is returned.
removeValue :: (Ord k, Ord v) => k -> v ->  MapSet k v ->  MapSet k v
removeValue k v (MapSet m) = MapSet $ Map.adjust (Set.removeElement v) k m

-- * ATOMIC QUERY FUNCTIONS
-------------------------------------------------------------------------------

-- | A list with all the different keys.
-- getKeys :: (Ord k, Ord v) => MapSet k v -> [k]
-- getKeys (MapSet m) = Map.keys m

-- | A set with the different values that exist for the key.
-- If key does not exist an empty Set is returned.
getValues :: (Ord k, Ord v) => k -> MapSet k v -> Set.Set v
getValues k (MapSet m) = Map.findWithDefault Set.empty k m

-- * UTILS FUNCTIONS
-------------------------------------------------------------------------------

isEmpty :: (Ord k, Ord v) => MapSet k v -> Bool
isEmpty (MapSet m) = Map.null m

-- | The number of different keys present.
getKeyCount :: (Ord k, Ord v) => MapSet k v -> Int
getKeyCount (MapSet m) = Map.size m

-- | All the different values that exist for the key.
getValuesList :: (Ord k, Ord v) => k -> MapSet k v -> [v]
getValuesList k mm = DCL.toList $ getValues k mm

-- | The number of different values that exist for the key.
getValueCount :: (Ord k, Ord v) => k -> MapSet k v -> Int
getValueCount k mm = fromInteger $ Set.getElementsCount $ getValues k mm

-- | Value exists for the key?
containsValue :: (Ord k, Ord v) => k -> v -> MapSet k v -> Bool
containsValue k v mm = Set.containsElement v $ getValues k mm

getValuesAndRemoveKey :: (Ord k, Ord v) => k -> MapSet k v -> (MapSet k v, [v])
getValuesAndRemoveKey k (MapSet m) = f $ Map.updateLookupWithKey (\_ _ -> Nothing) k m where
	f (Nothing, m) = (MapSet m, [])
	f (Just v, m) = (MapSet m, DCL.toList v)

removeValueFromKeys :: (Ord k, Ord v) => [k] -> v -> MapSet k v -> MapSet k v
removeValueFromKeys ks v (MapSet m) = MapSet (Map.unionWith f m m') where
	f set _ = Set.removeElement v set
	m' = Map.fromList $ map g ks where
		g k = (k, Set.empty)

-- | Removes all the values from the key, the key is retained with no values.
-- If key does not exist the original MapSet is returned.
-- If there are no values the original MapSet is returned.
removeValuesAll :: (Ord k, Ord v) => k -> MapSet k v ->  MapSet k v
removeValuesAll k (MapSet m) = MapSet $ Map.adjust (const Set.empty) k m

mapSet :: (Set.Set v -> Set.Set v') -> MapSet k v -> MapSet k v'
mapSet f (MapSet mm) = MapSet (Map.map f mm)

foldSet :: (Ord k, Ord v) => (Set.Set v -> ans -> ans) -> ans -> MapSet k v -> ans
foldSet f ans (MapSet mm) = Map.fold f ans mm

foldSetWithKey :: (Ord k, Ord v) => (k -> Set.Set v -> ans -> ans) -> ans -> MapSet k v -> ans
foldSetWithKey f ans (MapSet mm) = Map.foldWithKey f ans mm

-- * QUERY FUNCTIONS
-------------------------------------------------------------------------------

-- TODO

-- INSTANCES
-------------------------------------------------------------------------------

instance (Ord k, Ord v) => DC.Collection (MapSet k v) where
	type DC.Element (MapSet k v) = (k, v)
	addElement = addElement
	removeElement = removeElement
	containsElement = containsElement
	getElementsCount = getElementsCount

instance (Ord k, Ord v) => DCL.List (MapSet k v) where
	toList = toList
	fromList = fromList

instance (Ord k, Ord v) => DCF.Foldable (MapSet k v) where
	foldr = foldrElements 
	foldl = foldlElements

instance (Ord k, Ord v) => DCM.Map (MapSet k v) where
	type DCM.Keys (MapSet k v) = Set.Set k
	type DCM.Value (MapSet k v) = Set.Set v
	putValue = putValue
	removeKey = removeKey
	getKeys = getKeys
	containsKey = containsKey
	getKeysCount = getKeysCount
	getValue = getValue

instance (Ord k, Ord v) => DCM.Combination (MapSet k v) where
	getValueMaybe = getValueMaybe
	getValueWithDefault = getValueWithDefault
	getValueAndRemoveKey = getValueAndRemoveKey

instance (Ord k, Ord v) => DCMF.Foldable (MapSet k v) where
	foldr = foldrSet
	foldl = foldlSet
	-- Default implementations for foldr'
	-- Default implementations for foldr'
	foldrWithKey = foldrSetWithKey
	foldlWithKey = foldlSetWithKey
	-- Default implementations for foldrWithKey'
	-- Default implementations for foldlWithKey'

instance (Ord k, Ord v) => DCMM.MultiMap (MapSet k v) where
	addKey = addKey
	addToKey = addValue
	removeFromKey = removeValue
	containedInKey = containsValue
	getValuesCount k mm = toInteger $ getValueCount k mm

instance (Ord k, Ord v) => DCMM.Batch (MapSet k v) where
	remove v (MapSet m) = MapSet (Map.map (DC.removeElement v) m)
	removeFromKeys = removeValueFromKeys

instance (Ord k, Ord v) => DCMM.Foldable (MapSet k v) where
	foldr f a (MapSet m) = Map.foldrWithKey g a m where
		g k set a = DCF.foldr f a set
	foldl f a (MapSet m) = Map.foldlWithKey g a m where
		g a k set = DCF.foldl f a set
	-- Default implementations for foldr'. TODO: Use DCM.Foldable to implement!
	-- Default implementations for foldr'. TODO: Use DCM.Foldable to implement!
	foldrWithKey f a (MapSet m) = Map.foldrWithKey g a m where
		g k set a = DCF.foldr f' a set where
			f' v a = f k v a
	foldlWithKey f a (MapSet m) = Map.foldlWithKey g a m where
		g a k set = DCF.foldl f' a set where
			f' a v = f a k v
	-- Default implementations for foldrWithKey'. TODO: Use DCM.Foldable to implement!
	-- Default implementations for foldlWithKey'. TODO: Use DCM.Foldable to implement!

instance (Ord k, Ord v) => DCMMS.MapSet (MapSet k v)

