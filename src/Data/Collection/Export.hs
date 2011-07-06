-- Author: Federico Mastellone (fmaste@gmail.com)

-- Collection classes.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Export (
	Export(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC

-- CLASSES
-------------------------------------------------------------------------------

-- List conversion functions.
class DC.Collection c => Export c where

	-- Export to a list representation.
	toList :: c -> [DC.Element c]

