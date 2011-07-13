-- Author: Federico Mastellone (fmaste@gmail.com)

-- Set class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Set (
	Set(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC

-- CLASS
-------------------------------------------------------------------------------

class DC.Collection s => Set s where
	
	-- Calculates the union of two Sets.
	getUnion :: s -> s -> s

	-- Calculates the intersection of two Sets.
	getIntersection :: s -> s -> s

