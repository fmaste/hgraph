-- Author: Federico Mastellone (fmaste@gmail.com)

-- Standard library Data.Set repackaged.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Data.Collection.Set.Default (
	Set,
	empty,
	addElement,
	removeElement,
	getElementCount,
	containsElement,
	getElementsList ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Set as DS
import qualified Data.Collection as DC

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

getElementCount :: Ord a => Set a -> Integer
getElementCount = toInteger . DS.size

containsElement :: Ord a => a -> Set a -> Bool
containsElement = DS.member

getElementsList :: Ord a => Set a -> [a]
getElementsList = DS.elems

-- INSTANCE
-------------------------------------------------------------------------------

instance Ord a => DC.Collection (Set a) where
	type DC.Element (Set a) = a
	empty = empty
	addElement = addElement
	removeElement = removeElement
	getElementCount = getElementCount
	containsElement = containsElement
	getElementsList = getElementsList

