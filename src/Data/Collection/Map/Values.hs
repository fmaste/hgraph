-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association values class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Map.Values (
	Values(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection.Map as DCM

-- CLASSES
-------------------------------------------------------------------------------

class DCM.Map m => Values m where

	-- All the values that are associated with a key.
	getValues :: m -> [DCM.Value m]

	-- True if the value is associated with any key, otherwise, false.
	containsValue :: DCM.Value m -> m -> Bool

	-- Remove the keys that have this associated value.
	removeValue :: DCM.Value m -> m -> m

