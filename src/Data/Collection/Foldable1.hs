-- Author: Federico Mastellone (fmaste@gmail.com)

-- Foldable Collection classes.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Foldable (
	Foldable1(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import qualified Data.Collection as DC
import qualified Data.Collection.Foldable as DCF

-- CLASSES
-------------------------------------------------------------------------------

-- Collections like Map and MapSet can't implement this functions, so they are separated from Foldable.
class DCF.Foldable c => Foldable1 c where

	-- A variant of foldr that has no base case.
	-- May only be applied to non-empty Collections.
	foldr1 :: (DC.Element c -> DC.Element c -> DC.Element c) -> c -> DC.Element c
	foldr1 f c = fromMaybe (error "foldr1: empty structure") (foldr mf Nothing c) where
		mf x Nothing = Just x
		mf x (Just y) = Just (f x y)

	-- A variant of foldl that has no base case.
	-- May only be applied to non-empty Collections.
	foldl1 :: (DC.Element c -> DC.Element c -> DC.Element c) -> c -> DC.Element c
	foldl1 f c = fromMaybe (error "foldl1: empty structure") (foldl mf Nothing c) where
		mf Nothing y = Just y
		mf (Just x) y = Just (f x y)

