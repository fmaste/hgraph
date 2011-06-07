-- Author: Federico Mastellone (fmaste@gmail.com)

-- Standard library Data.Set repackaged.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Data.Collection.Set.Standard (
	Set,
	empty,
	addElement,
	removeElement,
	containsElement,
	getElementsCount,
	getElementsList,
	toList,
	getUnion,
	getIntersection) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Set as DS
import qualified Data.Collection as DC
import qualified Data.Collection.Set as DCS

-- DATA DEFINITION
-------------------------------------------------------------------------------

type Set = DS.Set

-- EXPORTED
-------------------------------------------------------------------------------

empty :: Ord a => Set a
empty = DS.empty

addElement :: Ord a => a -> Set a -> Set a
addElement = DS.insert

removeElement :: Ord a => a -> Set a -> Set a
removeElement = DS.delete

containsElement :: Ord a => a -> Set a -> Bool
containsElement = DS.member

getElementsCount :: Set a -> Integer
getElementsCount = toInteger . DS.size

getElementsList :: Set a -> [a]
getElementsList = DS.elems

toList :: Set a -> [a]
toList = getElementsList

getUnion :: Ord a => Set a -> Set a -> Set a
getUnion = DS.union

getIntersection :: Ord a => Set a -> Set a -> Set a
getIntersection = DS.intersection

-- INSTANCE
-------------------------------------------------------------------------------

instance Ord a => DC.Collection (Set a) where
	type DC.Element (Set a) = a
	addElement = addElement
	removeElement = removeElement
	containsElement = containsElement
	getElementsCount = getElementsCount
	getElementsList = getElementsList

instance Ord a => DC.CollectionList (Set a) where
	toList = toList

instance Ord a => DCS.Set (Set a) where
	getUnion = getUnion
	getIntersection = getIntersection

