-- Author: Federico Mastellone (fmaste@gmail.com)

-- Collection classes.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Batch (
	Batch(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC

-- CLASSES
-------------------------------------------------------------------------------

-- Performant functions to operate on more than one element.
-- No default implementations because they must be performant.
-- Use fold if your Collection if not an instance and you need this functions.
class DC.Collection c => Batch c where

	-- Same as addElement multiple times but faster.
	addElements :: [DC.Element c] -> c -> c

	-- Same as removeElement multiple times but faster.
	removeElements :: [DC.Element c] -> c -> c

	-- Same as containsElement multiple times but faster.
	containsElements :: [DC.Element c] -> c -> Bool

