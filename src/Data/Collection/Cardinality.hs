-- Author: Federico Mastellone (fmaste@gmail.com)

-- Collection classes.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Cardinality (
	Cardinality(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC

-- CLASSES
-------------------------------------------------------------------------------

-- List conversion functions.
class DC.Collection c => Cardinality c where

	-- The number of Elements that the Collection contains.
	getElementsCount :: c -> Integer

