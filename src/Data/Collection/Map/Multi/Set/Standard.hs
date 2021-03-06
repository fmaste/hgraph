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
	getValue,
	containsKey,
	getKeysCount,
	getValueWithDefault,
	getValueAndRemoveKey,
	foldrSet,
	foldlSet,
	foldrSet',
	foldlSet',
	foldrSetWithKey,
	foldlSetWithKey,
	foldrSetWithKey',
	foldlSetWithKey',
	addKey,
	addToKey,
	removeFromKey,
	containedInKey,
	getValuesCount,
	remove,
	removeFromKeys,
	foldr,
	foldl,
	foldr',
	foldl',
	foldrWithKey,
	foldlWithKey,
	foldrWithKey',
	foldlWithKey' ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Prelude hiding (foldr, foldl)
import qualified Data.List as DL
import qualified Data.Collection as DC
import qualified Data.Collection.Cardinality as DCC
import qualified Data.Collection.Import as DCI
import qualified Data.Collection.Export as DCE
import qualified Data.Collection.Foldable as DCF
import qualified Data.Collection.Map as DCM
import qualified Data.Collection.Map.Keys as DCMK
import qualified Data.Collection.Map.Foldable as DCMF
import qualified Data.Collection.Map.Multi as DCMM
import qualified Data.Collection.Map.Multi.Foldable as DCMMF
import qualified Data.Collection.Map.Multi.Set as DCMMS
import qualified Data.Collection.Map.Standard as Map
import qualified Data.Collection.Set.Standard as Set

-- * DATA DEFINITION
-------------------------------------------------------------------------------

newtype MapSet k v = MapSet (Map.Map k (Set.Set v))
	deriving (Show, Read, Ord, Eq)

-- EXPORTED
-------------------------------------------------------------------------------

-- | The empty MapSet.
empty :: (Ord k, Ord v) => MapSet k v
empty = MapSet $ Map.empty

addElement :: (Ord k, Ord v) => (k, v) -> MapSet k v -> MapSet k v
addElement (k, v) = addToKey k v

removeElement :: (Ord k, Ord v) => (k, v) -> MapSet k v -> MapSet k v
removeElement (k, v) = removeFromKey k v

containsElement :: (Ord k, Ord v) => (k, v) -> MapSet k v -> Bool
containsElement (k, v) m = Set.containsElement v $ getValueWithDefault Set.empty k m

getElementsCount :: (Ord k, Ord v) => MapSet k v -> Integer
getElementsCount (MapSet mm) = toInteger $ Map.foldr (\set ans -> ans + (Set.getElementsCount set)) 0 mm

toList :: (Ord k, Ord v) => MapSet k v -> [(k, v)]
toList (MapSet mm) = Map.foldrWithKey (\k set ans -> ans ++ [(k, v) | v <- (DCE.toList set)]) [] mm

fromList :: (Ord k, Ord v) => [(k, v)] -> MapSet k v
fromList list = DL.foldl' (\ans (k, v) -> addToKey k v ans) empty list

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
putValue k vSet (MapSet m) = MapSet (Map.putValue k vSet m)

-- | Removes the key and all its values.
-- If the key does not exists the original MapSet is returned.
removeKey :: (Ord k, Ord v) => k -> MapSet k v -> MapSet k v
removeKey k (MapSet m) = MapSet $ Map.removeKey k m

-- | A set with all the different keys.
getKeys :: (Ord k, Ord v) => MapSet k v -> [k]
getKeys (MapSet m) = Map.getKeys m

-- | A set with the different values that exist for the key.
-- If key does not exist an empty Set is returned.
getValue :: (Ord k, Ord v) => k -> MapSet k v -> Maybe (Set.Set v)
getValue k (MapSet m) = Map.getValue k m

-- | Key exists?
containsKey :: (Ord k, Ord v) => k -> MapSet k v -> Bool
containsKey k (MapSet m) = Map.containsKey k m

getKeysCount :: MapSet k v -> Integer
getKeysCount (MapSet m) = toInteger $ Map.getKeysCount m

getValueWithDefault :: (Ord k, Ord v) => Set.Set v -> k -> MapSet k v -> Set.Set v
getValueWithDefault v k (MapSet mm) = Map.getValueWithDefault v k mm

getValueAndRemoveKey :: (Ord k, Ord v) => k -> MapSet k v -> (Maybe (Set.Set v), MapSet k v)
getValueAndRemoveKey k mm = (getValue k mm, removeKey k mm)

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
addKey k (MapSet m) = MapSet $ Map.alter f k m where
	f (Just v) = Just v
	f Nothing = Just Set.empty

-- | Adds a value to key.
-- If key does not exist it is added.
-- If the value already exists the orignal MapSet is returned.
addToKey :: (Ord k, Ord v) => k -> v -> MapSet k v -> MapSet k v
addToKey k v (MapSet m) = MapSet $ Map.alter f k m where
	f (Just set) = Just (Set.addElement v set)
	f Nothing = Just (Set.addElement v Set.empty)

-- | Removes the value from the key.
-- If key does not exist the original MapSet is returned.
-- If the value does not exists the original MapSet is returned.
removeFromKey :: (Ord k, Ord v) => k -> v ->  MapSet k v ->  MapSet k v
removeFromKey k v (MapSet m) = MapSet $ Map.alter f k m where
	f (Just set) = Just (Set.removeElement v set)
	f Nothing = Nothing

-- | Value exists for the key?
containedInKey :: (Ord k, Ord v) => k -> v -> MapSet k v -> Bool
containedInKey k v mm = Set.containsElement v $ getValueWithDefault Set.empty k mm

-- | The number of different values that exist for the key.
getValuesCount :: (Ord k, Ord v) => k -> MapSet k v -> Integer
getValuesCount k mm = Set.getElementsCount $ getValueWithDefault Set.empty k mm

remove :: Ord v => v -> MapSet k v -> MapSet k v
remove v (MapSet m) = MapSet (Map.map (DC.removeElement v) m)

removeFromKeys :: (Ord k, Ord v) => [k] -> v -> MapSet k v -> MapSet k v
removeFromKeys ks v mm = DL.foldr f mm ks where
	f k mm = removeFromKey k v mm
{-- TODO: Efficient version
removeFromKeys ks v (MapSet m) = MapSet (Map.unionWith f m m') where
	f set _ = Set.removeElement v set
	m' = Map.fromList $ map g ks where
		g k = (k, Set.empty)
--}

foldr :: Ord v => (v -> a -> a) -> a -> MapSet k v -> a
foldr f a (MapSet m) = Map.foldrWithKey g a m where
	g k set a = DCF.foldr f a set

foldl :: Ord v => (a -> v -> a) -> a -> MapSet k v -> a
foldl f a (MapSet m) = Map.foldlWithKey g a m where
	g a k set = DCF.foldl f a set

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldr' :: (Ord k, Ord v) => (v -> a -> a) -> a -> MapSet k v -> a
foldr' = DCMMF.foldr' -- Use provided default implementation.

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldl' :: (Ord k, Ord v) => (a -> v -> a) -> a -> MapSet k v -> a
foldl' = DCMMF.foldl' -- Use provided default implementation.

foldrWithKey :: Ord v => (k -> v -> a -> a) -> a -> MapSet k v -> a
foldrWithKey f a (MapSet m) = Map.foldrWithKey g a m where
	g k set a = DCF.foldr f' a set where
		f' v a = f k v a

foldlWithKey :: Ord v => (a -> k -> v -> a) -> a -> MapSet k v -> a
foldlWithKey f a (MapSet m) = Map.foldlWithKey g a m where
	g a k set = DCF.foldl f' a set where
		f' a v = f a k v

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldrWithKey' :: (Ord k, Ord v) => (k -> v -> a -> a) -> a -> MapSet k v -> a
foldrWithKey' = DCMMF.foldrWithKey' -- Use provided default implementation.

-- TODO: Move the default implementation so I can remove the Ord contexts.
foldlWithKey' :: (Ord k, Ord v) => (a -> k -> v -> a) -> a -> MapSet k v -> a
foldlWithKey' = DCMMF.foldlWithKey' -- Use provided default implementation.

-- INSTANCES
-------------------------------------------------------------------------------

instance (Ord k, Ord v) => DC.Collection (MapSet k v) where
	type DC.Element (MapSet k v) = (k, v)
	addElement = addElement
	removeElement = removeElement
	containsElement = containsElement

instance (Ord k, Ord v) => DCC.Cardinality (MapSet k v) where
	getElementsCount = getElementsCount

instance (Ord k, Ord v) => DCI.Import (MapSet k v) where
	fromList = fromList

instance (Ord k, Ord v) => DCE.Export (MapSet k v) where
	toList = toList

instance (Ord k, Ord v) => DCF.Foldable (MapSet k v) where
	foldr = foldrElements 
	foldl = foldlElements
	-- Default implementations for foldr'
	-- Default implementations for foldl'

instance (Ord k, Ord v) => DCM.Map (MapSet k v) where
	type DCM.Key (MapSet k v) = k
	type DCM.Value (MapSet k v) = Set.Set v
	putValue = putValue
	removeKey = removeKey
	getKeys = getKeys
	getValue = getValue

instance (Ord k, Ord v) => DCMK.Keys (MapSet k v) where
	containsKey = containsKey
	getKeysCount = getKeysCount

instance (Ord k, Ord v) => DCM.Combination (MapSet k v) where
	getValueWithDefault = getValueWithDefault
	getValueAndRemoveKey = getValueAndRemoveKey

instance (Ord k, Ord v) => DCMF.Foldable (MapSet k v) where
	foldr = foldrSet
	foldl = foldlSet
	-- Default implementations for foldr'
	-- Default implementations for foldl'
	foldrWithKey = foldrSetWithKey
	foldlWithKey = foldlSetWithKey
	-- Default implementations for foldrWithKey'
	-- Default implementations for foldlWithKey'

instance (Ord k, Ord v) => DCMM.MultiMap (MapSet k v) where
	addKey = addKey
	addToKey = addToKey
	removeFromKey = removeFromKey
	containedInKey = containedInKey
	getValuesCount = getValuesCount

instance (Ord k, Ord v) => DCMM.Batch (MapSet k v) where
	remove = remove
	removeFromKeys = removeFromKeys

instance (Ord k, Ord v) => DCMMF.Foldable (MapSet k v) where
	foldr = foldr
	foldl = foldl
	-- Default implementations for foldr'
	-- Default implementations for foldl'
	foldrWithKey = foldrWithKey
	foldlWithKey = foldlWithKey
	-- Default implementations for foldrWithKey'
	-- Default implementations for foldlWithKey'

instance (Ord k, Ord v) => DCMMS.MapSet (MapSet k v)

