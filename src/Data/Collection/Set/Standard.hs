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
	toList,
	fromList,
	foldr,
	foldl,
	foldr',
	foldl',
	getUnion,
	getIntersection) where

-- IMPORTS
-------------------------------------------------------------------------------

import Prelude hiding (foldr, foldl)
import qualified Data.Set as DS
import qualified Data.Foldable as DF
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

toList :: Set a -> [a]
toList = DS.elems

fromList :: Ord a => [a] -> Set a
fromList = DS.fromList

foldr :: (a -> b -> b) -> b -> Set a -> b
foldr = DF.foldr

foldl :: (b -> a -> b) -> b -> Set a -> b
foldl = DF.foldl

foldr' :: (a -> b -> b) -> b -> Set a -> b
foldr' = DF.foldr'

foldl' :: (b -> a -> b) -> b -> Set a -> b
foldl' = DF.foldl'

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

instance Ord a => DC.List (Set a) where
	toList = toList
	fromList = fromList

instance Ord a => DC.Foldable (Set a) where
	foldr = foldr
	foldl = foldl
	foldr' = foldr'
	foldl' = foldl'

instance Ord a => DCS.Set (Set a) where
	getUnion = getUnion
	getIntersection = getIntersection

