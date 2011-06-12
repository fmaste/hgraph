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
	containsElement,
	getElementsCount,
	toList,
	fromList,
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

containsElement :: Int -> IntSet -> Bool
containsElement = DI.member

getElementsCount :: IntSet -> Integer
getElementsCount = toInteger . DI.size

toList :: IntSet -> [Int]
toList = DI.elems

fromList :: [Int] -> IntSet
fromList = DI.fromList

getUnion :: IntSet -> IntSet -> IntSet
getUnion = DI.union

getIntersection :: IntSet -> IntSet -> IntSet
getIntersection = DI.intersection

-- INSTANCE
-------------------------------------------------------------------------------

instance DC.Collection IntSet where
	type DC.Element IntSet = Int
	addElement = addElement
	removeElement = removeElement
	containsElement = containsElement
	getElementsCount = getElementsCount

instance DC.List IntSet where
	toList = toList
	fromList = fromList

instance DCS.Set IntSet where
	getUnion = getUnion
	getIntersection = getIntersection
