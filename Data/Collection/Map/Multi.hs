-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association of the type (Key, Collection Value) class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Map.Multi (
	MultiMap(..),
	Batch(..)  ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC
import qualified Data.Collection.Map as DCM

-- CLASSES
-------------------------------------------------------------------------------

class (DCM.Map a, DC.Collection (DCM.Value a)) => MultiMap a where

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- Adds a key and associate it to an empty collection.
	-- If the key already exists the original MultiMap is returned.
	addKey :: DC.Element (DCM.Keys a) -> a -> a

	-- Adds a collection value to key.
	-- If key does not exist it is added.
	addToKey :: DC.Element (DCM.Keys a) -> DC.Element (DCM.Value a) -> a -> a

	-- Removes the collection value from the key.
	-- If key does not exist the original MultiMap is returned.
	-- If the collection value does not exists the original MultiMap is returned.
	removeFromKey :: DC.Element (DCM.Keys a) -> DC.Element (DCM.Value a) -> a -> a

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------
	
	-- Collection value exists for the key?
	containedInKey :: DC.Element (DCM.Keys a) -> DC.Element (DCM.Value a) -> a -> Bool

	-- The number of collection values that the key has.
	getValuesCount :: DC.Element (DCM.Keys a) -> a -> Integer

-------------------------------------------------------------------------------

-- Performant functions to operate on more than one element.
-- No default implementations because they must be performant.
-- Use fold if your MultiMap if not an instance and you need this functions.
class MultiMap a => Batch a where

	-- Remove the value from the provided keys.
	removeFromKeys :: [DC.Element (DCM.Keys a)] -> DC.Element (DCM.Value a) -> a -> a

