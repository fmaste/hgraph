-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association of the type (Key, Collection Value) class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Map.Multi (
	MultiMap(..)  ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC
import qualified Data.Collection.Map as DCM

-- CLASS
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

