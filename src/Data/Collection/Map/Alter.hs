-- Author: Federico Mastellone (fmaste@gmail.com)

-- Map alter class.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Map.Alter (
	Alter(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection.Map as DCM

-- CLASSES
-------------------------------------------------------------------------------

class DCM.Map m => Alter m where

	-- The expression (alter f k m) alters the value x at k, or absence thereof. 
	-- Alter can be used to insert, delete, or update a value in a Map.
	alter :: (Maybe (DCM.Value m) -> Maybe (DCM.Value m)) -> DCM.Key m -> m -> m

