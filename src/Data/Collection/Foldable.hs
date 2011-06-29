-- Author: Federico Mastellone (fmaste@gmail.com)

-- Foldable Collection classes.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Foldable (
	Foldable(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Prelude hiding (foldr, foldl)
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

