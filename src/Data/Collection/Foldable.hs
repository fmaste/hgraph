-- Author: Federico Mastellone (fmaste@gmail.com)

-- Foldable Collection classes.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Foldable (
	Foldable(..),
	Foldable1(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Prelude hiding (foldr, foldl)
import Data.Maybe (fromMaybe)
import qualified Data.Collection as DC

-- CLASSES
-------------------------------------------------------------------------------

-- Foldable class for Collections.
class DC.Collection c => Foldable c where

	-- Right-associative fold of a Collection.
	foldr :: (DC.Element c -> a -> a) -> a -> c -> a

	-- Left-associative fold of a Collection.
	foldl :: (a -> DC.Element c -> a) -> a -> c -> a

	-- Fold over the elements of a Collection, associating to the right, but strictly.
	foldr' :: (DC.Element c -> a -> a) -> a -> c -> a
	foldr' = defaultFoldr'

	-- Fold over the elements of a Collection, associating to the left, but strictly.
	foldl' :: (a -> DC.Element c -> a) -> a -> c -> a
	foldl' = defaultFoldl'

defaultFoldr' :: (DC.Collection c, Foldable c) => (DC.Element c -> a -> a) -> a -> c -> a
defaultFoldr' f z0 xs = foldl f' id xs z0 where 
	f' g x z = g $! f x z

defaultFoldl' :: (DC.Collection c, Foldable c) => (a -> DC.Element c -> a) -> a -> c -> a
defaultFoldl' f z0 xs = foldr f' id xs z0 where 
	f' x g z = g $! f z x

-------------------------------------------------------------------------------

-- Collections like Map and MapSet can't implement this functions, so they are separated from Foldable.
class Foldable c => Foldable1 c where

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

