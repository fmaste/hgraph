-- Author: Federico Mastellone (fmaste@gmail.com)

-- Bag class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Bag (
	Bag(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC

-- CLASS
-------------------------------------------------------------------------------

class DC.Collection b => Bag b

