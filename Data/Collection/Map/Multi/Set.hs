-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association of the type (Key, Set Value) class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Map.Multi.Set (
	DC.Collection(..),
	DCM.Map(..),
	DCMM.MultiMap(..),
	MapSet(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC
import qualified Data.Collection.Set as DCS
import qualified Data.Collection.Map as DCM
import qualified Data.Collection.Map.Multi as DCMM

-- CLASS
-------------------------------------------------------------------------------

class (DCMM.MultiMap a, DCS.Set (DCM.Value a)) => MapSet a

