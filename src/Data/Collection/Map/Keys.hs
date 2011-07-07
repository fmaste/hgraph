-- Author: Federico Mastellone (fmaste@gmail.com)

-- Association keys class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Map.Keys (
	Keys(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection.Map as DCM

-- CLASSES
-------------------------------------------------------------------------------

class DCM.Map m => Keys m where

	-- True if the key has an associated value, otherwise, false.
	containsKey :: DCM.Key m -> m -> Bool

	-- The number of keys that have an associated value.
	getKeysCount :: m -> Integer

