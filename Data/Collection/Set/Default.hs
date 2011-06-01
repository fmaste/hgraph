-- Author: Federico Mastellone (fmaste@gmail.com)

-- Standard library Data.Set repackaged.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Data.Collection.Set.Default (
	Set,
	empty,
	insert,
	delete,
	size,
	member,
	elems ) where

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

insert :: Ord a => a -> Set a -> Set a
insert = DS.insert

delete :: Ord a => a -> Set a -> Set a
delete = DS.delete

size :: Ord a => Set a -> Integer
size = toInteger . DS.size

member :: Ord a => a -> Set a -> Bool
member = DS.member

elems :: Ord a => Set a -> [a]
elems = DS.elems

-- INSTANCE
-------------------------------------------------------------------------------

instance Ord a => DC.Collection (Set a) where
	type DC.Element (Set a) = a
	empty = DS.empty
	insert = DS.insert
	delete = DS.delete
	size = toInteger . DS.size
	member = DS.member
	elems = DS.elems

