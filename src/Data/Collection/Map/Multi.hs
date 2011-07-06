-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association of the type (Key, Collection Value) class.
-- TODO: Make it haddock compatibele!

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

