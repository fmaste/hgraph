-- Author: Federico Mastellone (fmaste@gmail.com)

-- Theory of a binary relation class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Relation.Binary.Theory (
	Theory(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection.Relation.Binary as DCRB

-- CLASS
-------------------------------------------------------------------------------

class DCRB.BinaryRelation r => Theory r where

	isInjective :: r -> Bool
	
	-- TODO: Add more functions

