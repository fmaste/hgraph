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
class DCMM.MultiMap mm => Foldable mm where

	-- Right-associative fold of a Map.
	foldr :: (DC.Element (DCM.Value mm) -> a -> a) -> a -> mm -> a

	-- Left-associative fold of a Map.
	foldl :: (a -> DC.Element (DCM.Value mm) -> a) -> a -> mm -> a

	-- Fold over the elements of a Map, associating to the right, but strictly.
	foldr' :: (DC.Element (DCM.Value mm) -> a -> a) -> a -> mm -> a
	foldr' = defaultFoldr'

	-- Fold over the elements of a Map, associating to the left, but strictly.
	foldl' :: (a -> DC.Element (DCM.Value mm) -> a) -> a -> mm -> a
	foldl' = defaultFoldl'

	-- Right-associative fold with key of a Map.
	foldrWithKey :: (DCM.Key mm -> DC.Element (DCM.Value mm) -> a -> a) -> a -> mm -> a

	-- Left-associative fold with key of a Map.
	foldlWithKey :: (a -> DCM.Key mm -> DC.Element (DCM.Value mm) -> a) -> a -> mm -> a

	-- Fold over the elements of a Map with key, associating to the right, but strictly.
	foldrWithKey' :: (DCM.Key mm -> DC.Element (DCM.Value mm) -> a -> a) -> a -> mm -> a
	foldrWithKey' = defaultFoldrWithKey'

	-- Fold over the elements of a Map with key, associating to the left, but strictly.
	foldlWithKey' :: (a -> DCM.Key mm -> DC.Element (DCM.Value mm) -> a) -> a -> mm -> a
	foldlWithKey' = defaultFoldlWithKey'

defaultFoldr' :: (DCMM.MultiMap mm, Foldable mm) => (DC.Element (DCM.Value mm) -> a -> a) -> a -> mm -> a
defaultFoldr' f z0 xs = foldl f' id xs z0 where 
	f' g x z = g $! f x z

defaultFoldl' :: (DCMM.MultiMap mm, Foldable mm) => (a -> DC.Element (DCM.Value mm) -> a) -> a -> mm -> a
defaultFoldl' f z0 xs = foldr f' id xs z0 where 
	f' x g z = g $! f z x

defaultFoldrWithKey' :: (DCMM.MultiMap mm, Foldable mm) => (DCM.Key mm -> DC.Element (DCM.Value mm) -> a -> a) -> a -> mm -> a
defaultFoldrWithKey' f z0 xs = foldlWithKey f' id xs z0 where 
	f' g k x z = g $! f k x z

defaultFoldlWithKey' :: (DCMM.MultiMap mm, Foldable mm) => (a -> DCM.Key mm -> DC.Element (DCM.Value mm) -> a) -> a -> mm -> a
defaultFoldlWithKey' f z0 xs = foldrWithKey f' id xs z0 where 
	f' k x g z = g $! f z k x

