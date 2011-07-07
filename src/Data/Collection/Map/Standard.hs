-- Author: Federico Mastellone (fmaste@gmail.com)

-- Standard library Data.Map repackaged.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Data.Collection.Map.Standard (
	-- Propietary.
	Map,
	empty,
	-- Collection functions.
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
	getValue,
	alter,
	containsKey,
	getKeysCount,
	getValueWithDefault,
	getValueAndRemoveKey,
	foldr,
	foldl,
	foldr',
	foldl',
	foldrWithKey,
	foldlWithKey,
	foldrWithKey',
	foldlWithKey',
	-- Extras.
	map ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Prelude hiding (map, foldr, foldl)
import qualified Data.Map as DM
import qualified Data.Collection as DC
import qualified Data.Collection.Cardinality as DCC
import qualified Data.Collection.Import as DCI
import qualified Data.Collection.Export as DCE
import qualified Data.Collection.Foldable as DCF
import qualified Data.Collection.Set.Standard as DCSS
import qualified Data.Collection.Map as DCM
import qualified Data.Collection.Map.Alter as DCMA
import qualified Data.Collection.Map.Keys as DCMK
import qualified Data.Collection.Map.Foldable as DCMF

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
containsElement (k, v) m = if getValue k m == Just v then True else False

getElementsCount :: Map k v -> Integer
getElementsCount = toInteger . DM.size

toList :: Map k v -> [(k, v)]
toList = DM.toList 

fromList :: Ord k => [(k, v)] -> Map k v
fromList = DM.fromList

-- Collection version of fold

foldrElements :: ((k, v) -> a -> a) -> a -> Map k v -> a
foldrElements f a m = DM.foldrWithKey g a m where
	g k v a = f (k, v) a

foldlElements :: (a -> (k, v) -> a) -> a -> Map k v -> a
foldlElements f a m = DM.foldlWithKey g a m where
	g a k v = f a (k, v)

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldrElements' :: (Ord k, Ord v) => ((k, v) -> a -> a) -> a -> Map k v -> a
foldrElements' = DCF.foldr' -- Use provided default implementation.

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldlElements' :: (Ord k, Ord v) => (a -> (k, v) -> a) -> a -> Map k v -> a
foldlElements' = DCF.foldl' -- Use provided default implementation.

putValue :: Ord k => k -> v -> Map k v -> Map k v
putValue = DM.insert

removeKey :: Ord k => k -> Map k v -> Map k v
removeKey = DM.delete

getKeys :: Map k v -> [k]
getKeys = DM.keys

getValue :: Ord k => k -> Map k v -> Maybe v
getValue = DM.lookup

alter :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter = DM.alter

containsKey :: Ord k => k -> Map k v -> Bool
containsKey = DM.member

getKeysCount :: Map k v -> Integer
getKeysCount m = toInteger $ DM.size m

getValueWithDefault :: Ord k => v -> k -> Map k v -> v
getValueWithDefault = DM.findWithDefault

getValueAndRemoveKey :: Ord k => k -> Map k v -> (Maybe v, Map k v)
getValueAndRemoveKey k m = DM.updateLookupWithKey (\_ _ -> Nothing) k m where

-- Map version of fold

foldr :: (v -> a -> a) -> a -> Map k v -> a
foldr f a m = DM.foldrWithKey g a m where
	g k v a = f v a

foldl :: (a -> v -> a) -> a -> Map k v -> a
foldl f a m = DM.foldlWithKey g a m where
	g a k v = f a v

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldr' :: (Ord k, Ord v) => (v -> a -> a) -> a -> Map k v -> a
foldr' = DCMF.foldr' -- Use provided default implementation.

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldl' :: (Ord k, Ord v) => (a -> v -> a) -> a -> Map k v -> a
foldl' = DCMF.foldl' -- Use provided default implementation.

foldrWithKey :: (k -> v -> a -> a) -> a -> Map k v -> a
foldrWithKey = DM.foldrWithKey

foldlWithKey :: (a -> k -> v -> a) -> a -> Map k v -> a
foldlWithKey = DM.foldlWithKey

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldrWithKey' :: (Ord k, Ord v) => (k -> v -> a -> a) -> a -> Map k v -> a
foldrWithKey' = DCMF.foldrWithKey' -- Use provided default implementation.

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldlWithKey' :: (Ord k, Ord v) => (a -> k -> v -> a) -> a -> Map k v -> a
foldlWithKey' = DCMF.foldlWithKey' -- Use provided default implementation.

map :: (a -> b) -> Map k a -> Map k b
map = DM.map

-- INSTANCES
-------------------------------------------------------------------------------

-- A collection of (k, v) tuples where if k exists, it must contain only one v.
instance (Ord k, Ord v) => DC.Collection (Map k v) where
	type DC.Element (Map k v) = (k, v)
	addElement = addElement
	removeElement = removeElement
	containsElement = containsElement

instance (Ord k, Ord v) => DCC.Cardinality (Map k v) where
	getElementsCount = getElementsCount

instance (Ord k, Ord v) => DCI.Import (Map k v) where
	fromList = fromList

instance (Ord k, Ord v) => DCE.Export (Map k v) where
	toList = toList

instance (Ord k, Ord v) => DCF.Foldable (Map k v) where
	foldr = foldrElements
	foldl = foldlElements
	-- Default implementations for foldr'
	-- Default implementations for foldr'

instance (Ord k, Ord v) => DCM.Map (Map k v) where
	type DCM.Key (Map k v) = k
	type DCM.Value (Map k v) = v
	putValue = putValue
	removeKey = removeKey
	getKeys = getKeys
	getValue = getValue

instance (Ord k, Ord v) => DCMA.Alter (Map k v) where
	alter = alter

instance (Ord k, Ord v) => DCMK.Keys (Map k v) where
	containsKey = containsKey
	getKeysCount = getKeysCount

instance (Ord k, Ord v) => DCM.Combination (Map k v) where
	getValueWithDefault = getValueWithDefault
	getValueAndRemoveKey = getValueAndRemoveKey

instance (Ord k, Ord v) => DCMF.Foldable (Map k v) where
	foldr = foldr
	foldl = foldl
	-- Default implementations for foldr'
	-- Default implementations for foldr'
	foldrWithKey = foldrWithKey
	foldlWithKey = foldlWithKey
	-- Default implementations for foldrWithKey'
	-- Default implementations for foldlWithKey'

