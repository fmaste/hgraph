-- Author: Federico Mastellone (fmaste@gmail.com)

-- Foldable MultiMap classes.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Map.Multi.Foldable (
	Foldable(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import Prelude hiding (foldr, foldl)
import qualified Data.Collection as DC
import qualified Data.Collection.Map as DCM
import qualified Data.Collection.Map.Multi as DCMM

-- CLASSES
-------------------------------------------------------------------------------

-- Foldable class for MultiMap.
class DCMM.MultiMap m => Foldable m where

	-- Right-associative fold of a Map.
	foldr :: (DC.Element (DCM.Value m) -> a -> a) -> a -> m -> a

	-- Left-associative fold of a Map.
	foldl :: (a -> DC.Element (DCM.Value m) -> a) -> a -> m -> a

	-- Fold over the elements of a Map, associating to the right, but strictly.
	foldr' :: (DC.Element (DCM.Value m) -> a -> a) -> a -> m -> a
	foldr' = defaultFoldr'

	-- Fold over the elements of a Map, associating to the left, but strictly.
	foldl' :: (a -> DC.Element (DCM.Value m) -> a) -> a -> m -> a
	foldl' = defaultFoldl'

	-- Right-associative fold with key of a Map.
	foldrWithKey :: (DC.Element (DCM.Keys m) -> DC.Element (DCM.Value m) -> a -> a) -> a -> m -> a

	-- Left-associative fold with key of a Map.
	foldlWithKey :: (a -> DC.Element (DCM.Keys m) -> DC.Element (DCM.Value m) -> a) -> a -> m -> a

	-- Fold over the elements of a Map with key, associating to the right, but strictly.
	foldrWithKey' :: (DC.Element (DCM.Keys m) -> DC.Element (DCM.Value m) -> a -> a) -> a -> m -> a
	foldrWithKey' = defaultFoldrWithKey'

	-- Fold over the elements of a Map with key, associating to the left, but strictly.
	foldlWithKey' :: (a -> DC.Element (DCM.Keys m) -> DC.Element (DCM.Value m) -> a) -> a -> m -> a
	foldlWithKey' = defaultFoldlWithKey'

defaultFoldr' :: (DCMM.MultiMap m, Foldable m) => (DC.Element (DCM.Value m) -> a -> a) -> a -> m -> a
defaultFoldr' f z0 xs = foldl f' id xs z0 where 
	f' g x z = g $! f x z

defaultFoldl' :: (DCMM.MultiMap m, Foldable m) => (a -> DC.Element (DCM.Value m) -> a) -> a -> m -> a
defaultFoldl' f z0 xs = foldr f' id xs z0 where 
	f' x g z = g $! f z x

defaultFoldrWithKey' :: (DCMM.MultiMap m, Foldable m) => (DC.Element (DCM.Keys m) -> DC.Element (DCM.Value m) -> a -> a) -> a -> m -> a
defaultFoldrWithKey' f z0 xs = foldlWithKey f' id xs z0 where 
	f' g k x z = g $! f k x z

defaultFoldlWithKey' :: (DCMM.MultiMap m, Foldable m) => (a -> DC.Element (DCM.Keys m) -> DC.Element (DCM.Value m) -> a) -> a -> m -> a
defaultFoldlWithKey' f z0 xs = foldrWithKey f' id xs z0 where 
	f' k x g z = g $! f z k x

