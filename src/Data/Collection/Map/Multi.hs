-- Author: Federico Mastellone (fmaste@gmail.com)

-- Multi associations class.
-- TODO: Make it haddock compatible!

-- An easy way to handle a (Map k v) where Value is a Collection.
-- You can think of it a Map (its superclass) where the Value is a Collection.
-- This way you can think about it as a binary relation that is a function from Keys to Collections.
-- Or you can handle the Collection Elements individually.
-- This way you can think about it as a binary relation from Keys to Elements.
-- The domain is a set of Keys where each Key is associated with zero or more Elements.
-- The codomain is a bag of Elements where each Value is associated with only one key.

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Map.Multi (
	MultiMap(..),
	Batch(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import qualified Data.Collection as DC
import qualified Data.Collection.Map as DCM

-- CLASSES
-------------------------------------------------------------------------------

class (DCM.Map mm, DC.Collection (DCM.Value mm)) => MultiMap mm where

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- Adds a key and associate it to an empty collection.
	-- If the key already exists the original MultiMap is returned.
	addKey :: DCM.Key mm -> mm -> mm

	-- Adds a collection value to key.
	-- If key does not exist it is added.
	addToKey :: DCM.Key mm -> DC.Element (DCM.Value mm) -> mm -> mm

	-- Removes the collection value from the key.
	-- If key does not exist the original MultiMap is returned.
	-- If the collection value does not exists the original MultiMap is returned.
	removeFromKey :: DCM.Key mm -> DC.Element (DCM.Value mm) -> mm -> mm

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------
	
	-- Collection value exists for the key?
	containedInKey :: DCM.Key mm -> DC.Element (DCM.Value mm) -> mm -> Bool

	-- The number of collection values that the key has.
	getValuesCount :: DCM.Key mm -> mm -> Integer

-------------------------------------------------------------------------------

-- Performant functions to operate on more than one element.
-- No default implementations because they must be performant.
-- Use fold if your MultiMap if not an instance and you need this functions.
class MultiMap mm => Batch mm where

	-- Remove the value from all the keys.
	remove :: DC.Element (DCM.Value mm) -> mm -> mm

	-- Remove the value from the provided keys.
	removeFromKeys :: [DCM.Key mm] -> DC.Element (DCM.Value mm) -> mm -> mm

