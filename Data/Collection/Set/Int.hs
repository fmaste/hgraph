-- Author: Federico Mastellone (fmaste@gmail.com)

-- Standard library Data.IntSet repackaged.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Data.Collection.Set.Int (
	IntSet,
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

import qualified Data.IntSet as DI
import qualified Data.Collection as DC
import qualified Data.Collection.Set as DCS

-- DATA DEFINITION
-------------------------------------------------------------------------------

type IntSet = DI.IntSet

-- EXPORTED
-------------------------------------------------------------------------------

empty :: IntSet
empty = DI.empty

addElement :: Int -> IntSet -> IntSet
addElement = DI.insert

removeElement :: Int -> IntSet -> IntSet
removeElement = DI.delete

getElementsCount :: IntSet -> Integer
getElementsCount = toInteger . DI.size

containsElement :: Int -> IntSet -> Bool
containsElement = DI.member

getElementsList :: IntSet -> [Int]
getElementsList = DI.elems

getUnion :: IntSet -> IntSet -> IntSet
getUnion = DI.union

getIntersection :: IntSet -> IntSet -> IntSet
getIntersection = DI.intersection

-- INSTANCE
-------------------------------------------------------------------------------

instance DC.Collection IntSet where
	type DC.Element IntSet = Int
	empty = empty
	addElement = addElement
	removeElement = removeElement
	getElementsCount = getElementsCount
	containsElement = containsElement
	getElementsList = getElementsList

instance DCS.Set IntSet where
	getUnion = getUnion
	getIntersection = getIntersection

