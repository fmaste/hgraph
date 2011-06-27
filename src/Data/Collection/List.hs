-- Author: Federico Mastellone (fmaste@gmail.com)

-- Collection classes.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.List (
	List(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC

-- CLASSES
-------------------------------------------------------------------------------

-- List conversion functions.
class DC.Collection c => List c where

	-- Export to a list representation.
	toList :: c -> [DC.Element c]

	-- Import from a list representation.
	fromList :: [DC.Element c] -> c

