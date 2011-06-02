-- Author: Federico Mastellone (fmaste@gmail.com)

-- Standard library Data.Map repackaged.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Data.Collection.Association.Standard (
	Map,
	empty,
	addValue,
	removeKey,
	containsKey,
	getAssociatedValue,
	getAssociationsCount,
	getAssociationsList ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Map as DM
import qualified Data.Collection as DC
import qualified Data.Collection.Association as DCA

-- DATA DEFINITION
-------------------------------------------------------------------------------

type Map = DM.Map

-- EXPORTED
-------------------------------------------------------------------------------

empty :: Map k v
empty = DM.empty

addValue :: Ord k => k -> v -> Map k v -> Map k v
addValue = DM.insert

removeKey :: Ord k => k -> Map k v -> Map k v
removeKey = DM.delete

containsKey :: Ord k => k -> Map k v -> Bool
containsKey = DM.member

getAssociatedValue :: Ord k => k -> Map k v -> Maybe v
getAssociatedValue = DM.lookup

getAssociationsCount :: Map k v -> Integer
getAssociationsCount = toInteger . DM.size

getAssociationsList :: Map k v -> [(k, v)]
getAssociationsList = DM.toList

-- INSTANCE
-------------------------------------------------------------------------------

-- A collection of (k, v) tuples where if k exists, it must contain only one v.
instance (Ord k, Ord v) => DC.Collection (Map k v) where
	type DC.Element (Map k v) = (k, v)
	empty = empty
	addElement (k, v) m = addValue k v m
	removeElement (k, v) m = DM.update f k m where
		f x = if x == v then Just x else Nothing
	getElementsCount = getAssociationsCount
	containsElement (k, v) m = if getAssociatedValue k m == Just v then True else False
	getElementsList = getAssociationsList

instance (Ord k, Ord v) => DCA.Association (Map k v) where
	type DCA.Key (Map k v) = k
	type DCA.Value (Map k v) = v
	addValue = addValue
	removeKey = removeKey
	containsKey = containsKey
	getAssociatedValue = getAssociatedValue
	getAssociationsCount = getAssociationsCount
	getAssociationsList = getAssociationsList

