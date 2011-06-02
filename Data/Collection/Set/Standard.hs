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
	getElementsCount,
	containsElement,
	getElementsList,
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

getElementsCount :: Set a -> Integer
getElementsCount = toInteger . DS.size

containsElement :: Ord a => a -> Set a -> Bool
containsElement = DS.member

getElementsList :: Set a -> [a]
getElementsList = DS.elems

getUnion :: Ord a => Set a -> Set a -> Set a
getUnion = DS.union

getIntersection :: Ord a => Set a -> Set a -> Set a
getIntersection = DS.intersection

-- INSTANCE
-------------------------------------------------------------------------------

instance Ord a => DC.Collection (Set a) where
	type DC.Element (Set a) = a
	empty = empty
	addElement = addElement
	removeElement = removeElement
	getElementsCount = getElementsCount
	containsElement = containsElement
	getElementsList = getElementsList

instance Ord a => DCS.Set (Set a) where
	getUnion = getUnion
	getIntersection = getIntersection

