-- Author: Federico Mastellone (fmaste@gmail.com)

-- Standard library Data.IntMap repackaged.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Data.Collection.Map.Int (
	-- Propietary.
	IntMap,
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
import qualified Data.IntMap as IntMap
import qualified Data.Foldable as Foldable
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

type IntMap = IntMap.IntMap

-- EXPORTED
-------------------------------------------------------------------------------

empty :: IntMap v
empty = IntMap.empty

addElement :: (Int, v) -> IntMap v -> IntMap v
addElement (k, v) = putValue k v

removeElement :: Ord v => (Int, v) -> IntMap v -> IntMap v
removeElement (k, v) m = IntMap.update f k m where
	f x = if x == v then Nothing else Just x

containsElement :: Ord v => (Int, v) -> IntMap v -> Bool
containsElement (k, v) m = if getValue k m == Just v then True else False

getElementsCount :: IntMap v -> Integer
getElementsCount = toInteger . IntMap.size

toList :: IntMap v -> [(Int, v)]
toList = IntMap.toList 

fromList :: [(Int, v)] -> IntMap v
fromList = IntMap.fromList

-- Collection version of fold

foldrElements :: ((Int, v) -> a -> a) -> a -> IntMap v -> a
-- TODO: No foldr
foldrElements f a m = IntMap.foldWithKey g a m where
	g k v a = f (k, v) a

foldlElements :: (a -> (Int, v) -> a) -> a -> IntMap v -> a
-- TODO: No foldl
foldlElements f a m = IntMap.foldWithKey g a m where
	g k v a = f a (k, v)

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldrElements' :: Ord v => ((Int, v) -> a -> a) -> a -> IntMap v -> a
foldrElements' = DCF.foldr' -- Use provided default implementation.

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldlElements' :: Ord v => (a -> (Int, v) -> a) -> a -> IntMap v -> a
foldlElements' = DCF.foldl' -- Use provided default implementation.

putValue :: Int -> v -> IntMap v -> IntMap v
putValue = IntMap.insert

removeKey :: Int -> IntMap v -> IntMap v
removeKey = IntMap.delete

getKeys :: IntMap v -> [Int]
getKeys = IntMap.keys

getValue :: Int -> IntMap v -> Maybe v
getValue = IntMap.lookup

alter :: (Maybe v -> Maybe v) -> Int -> IntMap v -> IntMap v
alter = IntMap.alter

containsKey :: Int -> IntMap v -> Bool
containsKey = IntMap.member

getKeysCount :: IntMap v -> Integer
getKeysCount m = toInteger $ IntMap.size m

getValueWithDefault :: v -> Int -> IntMap v -> v
getValueWithDefault = IntMap.findWithDefault

getValueAndRemoveKey :: Int -> IntMap v -> (Maybe v, IntMap v)
getValueAndRemoveKey k m = IntMap.updateLookupWithKey (\_ _ -> Nothing) k m where

-- Map version of fold

foldr :: (v -> a -> a) -> a -> IntMap v -> a
foldr f a m = Foldable.foldr f a m

foldl :: (a -> v -> a) -> a -> IntMap v -> a
foldl f a m = Foldable.foldl f a m

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldr' :: Ord v => (v -> a -> a) -> a -> IntMap v -> a
foldr' = DCMF.foldr' -- Use provided default implementation.

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldl' :: Ord v => (a -> v -> a) -> a -> IntMap v -> a
foldl' = DCMF.foldl' -- Use provided default implementation.

foldrWithKey :: (Int -> v -> a -> a) -> a -> IntMap v -> a
-- TODO: No foldr
foldrWithKey f a m = IntMap.foldWithKey g a m where
	g k v a = f k v a

foldlWithKey :: (a -> Int -> v -> a) -> a -> IntMap v -> a
-- TODO: No foldl
foldlWithKey f a m = IntMap.foldWithKey g a m where
	g k v a = f a k v

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldrWithKey' :: Ord v => (Int -> v -> a -> a) -> a -> IntMap v -> a
foldrWithKey' = DCMF.foldrWithKey' -- Use provided default implementation.

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldlWithKey' :: Ord v => (a -> Int -> v -> a) -> a -> IntMap v -> a
foldlWithKey' = DCMF.foldlWithKey' -- Use provided default implementation.

map :: (a -> b) -> IntMap a -> IntMap b
map = IntMap.map

-- INSTANCES
-------------------------------------------------------------------------------

-- A collection of (k, v) tuples where if k exists, it must contain only one v.
instance Ord v => DC.Collection (IntMap v) where
	type DC.Element (IntMap v) = (Int, v)
	addElement = addElement
	removeElement = removeElement
	containsElement = containsElement

instance Ord v => DCC.Cardinality (IntMap v) where
	getElementsCount = getElementsCount

instance Ord v => DCI.Import (IntMap v) where
	fromList = fromList

instance Ord v => DCE.Export (IntMap v) where
	toList = toList

instance Ord v => DCF.Foldable (IntMap v) where
	foldr = foldrElements
	foldl = foldlElements
	-- Default implementations for foldr'
	-- Default implementations for foldr'

instance Ord v => DCM.Map (IntMap v) where
	type DCM.Key (IntMap v) = Int
	type DCM.Value (IntMap v) = v
	putValue = putValue
	removeKey = removeKey
	getKeys = getKeys
	getValue = getValue

instance Ord v => DCMA.Alter (IntMap v) where
	alter = alter

instance Ord v => DCMK.Keys (IntMap v) where
	containsKey = containsKey
	getKeysCount = getKeysCount

instance Ord v => DCM.Combination (IntMap v) where
	getValueWithDefault = getValueWithDefault
	getValueAndRemoveKey = getValueAndRemoveKey

instance Ord v => DCMF.Foldable (IntMap v) where
	foldr = foldr
	foldl = foldl
	-- Default implementations for foldr'
	-- Default implementations for foldr'
	foldrWithKey = foldrWithKey
	foldlWithKey = foldlWithKey
	-- Default implementations for foldrWithKey'
	-- Default implementations for foldlWithKey'

