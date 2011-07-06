-- Author: Federico Mastellone (fmaste@gmail.com)

-- Collection alter class.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Alter (
	Alter(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC

-- CLASSES
-------------------------------------------------------------------------------

class DC.Collection c => Alter c where

	-- The expression (alter f e c) alters the value e at c, or absence thereof. 
	-- Alter can be used to insert, delete, or update a value in a Collection.
	alter :: (Maybe (DC.Element c) -> Maybe (DC.Element c)) -> DC.Element c -> c -> c

