-- Author: Federico Mastellone (fmaste@gmail.com)

-- Standard library Data.Map repackaged.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Data.Collection.Map.Standard (
	Map,
	empty,
	addElement,
	removeElement,
	containsElement,
	getElementsCount,
	toList,
	fromList,
	foldrElements,
	foldlElements,
	putValue,
	removeKey,
	getKeys,
	containsKey,
	getKeysCount,
	getValue,
	getValueMaybe,
	getValueWithDefault,
	getValueAndRemoveKey ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Map as DM
import qualified Data.Collection as DC
import qualified Data.Collection.Set.Standard as DCSS
import qualified Data.Collection.Map as DCM

-- DATA DEFINITION
-------------------------------------------------------------------------------

type Map = DM.Map

-- EXPORTED
-------------------------------------------------------------------------------

empty :: Map k v
empty = DM.empty

addElement :: Ord k => (k, v) -> Map k v -> Map k v
addElement (k, v) = putValue k v

removeElement :: (Ord k, Ord v) => (k, v) -> Map k v -> Map k v
removeElement (k, v) m = DM.update f k m where
	f x = if x == v then Just x else Nothing

containsElement :: (Ord k, Ord v) => (k, v) -> Map k v -> Bool
containsElement (k, v) m = if getValueMaybe k m == Just v then True else False

getElementsCount :: Map k v -> Integer
getElementsCount = toInteger . DM.size

toList :: Map k v -> [(k, v)]
toList = DM.toList 

fromList :: Ord k => [(k, v)] -> Map k v
fromList = DM.fromList

foldrElements :: ((k, v) -> a -> a) -> a -> Map k v -> a
foldrElements f a m = DM.foldrWithKey g a m where
	g k v a = f (k, v) a

foldlElements :: (a -> (k, v) -> a) -> a -> Map k v -> a
foldlElements f a m = DM.foldlWithKey g a m where
	g a k v = f a (k, v)

putValue :: Ord k => k -> v -> Map k v -> Map k v
putValue = DM.insert

removeKey :: Ord k => k -> Map k v -> Map k v
removeKey = DM.delete

getKeys :: Map k v -> DCSS.Set k 
getKeys = DM.keysSet

containsKey :: Ord k => k -> Map k v -> Bool
containsKey = DM.member

getKeysCount :: Map k v -> Integer
getKeysCount m = toInteger $ DM.size m

getValue :: Ord k => k -> Map k v -> v
getValue k m = m DM.! k

getValueMaybe :: Ord k => k -> Map k v -> Maybe v
getValueMaybe = DM.lookup

getValueWithDefault :: Ord k => v -> k -> Map k v -> v
getValueWithDefault = DM.findWithDefault

getValueAndRemoveKey :: Ord k => k -> Map k v -> (Maybe v, Map k v)
getValueAndRemoveKey k m = DM.updateLookupWithKey (\_ _ -> Nothing) k m where

-- INSTANCES
-------------------------------------------------------------------------------

-- A collection of (k, v) tuples where if k exists, it must contain only one v.
instance (Ord k, Ord v) => DC.Collection (Map k v) where
	type DC.Element (Map k v) = (k, v)
	addElement = addElement
	removeElement = removeElement
	containsElement = containsElement
	getElementsCount = getElementsCount

instance (Ord k, Ord v) => DC.List (Map k v) where
	toList = toList
	fromList = fromList

instance (Ord k, Ord v) => DC.Foldable (Map k v) where
	foldr = foldrElements
	foldl = foldlElements
	-- Default implementations for foldr1
	-- Default implementations for foldl1

instance (Ord k, Ord v) => DCM.Map (Map k v) where
	type DCM.Keys (Map k v) = DCSS.Set k
	type DCM.Value (Map k v) = v
	putValue = putValue
	removeKey = removeKey
	getKeys = getKeys
	containsKey = containsKey
	getKeysCount = getKeysCount
	getValue = getValue

instance (Ord k, Ord v) => DCM.Combination (Map k v) where
	getValueMaybe = getValueMaybe
	getValueWithDefault = getValueWithDefault
	getValueAndRemoveKey = getValueAndRemoveKey

