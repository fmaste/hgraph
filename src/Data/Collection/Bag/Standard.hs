-- Author: Federico Mastellone (fmaste@gmail.com)

-- Ord bag.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Bag.Standard (
	Bag,
	empty,
	addElement,
	removeElement,
	containsElement,
	getElementsCount,
	toList,
	fromList,
	foldr,
	foldl,
	foldr',
	foldl') where

-- IMPORTS
-------------------------------------------------------------------------------

import Prelude hiding (foldr, foldl)
import qualified Data.List as DL
import qualified Data.Collection as DC
import qualified Data.Collection.Cardinality as DCC
import qualified Data.Collection.Import as DCI
import qualified Data.Collection.Export as DCE
import qualified Data.Collection.Foldable as DCF
import qualified Data.Collection.Bag as DCB
import qualified Data.Collection.Map.Keys as DCMK
import qualified Data.Collection.Map.Alter as DCMA
import qualified Data.Collection.Map.Foldable as DCMF
import qualified Data.Collection.Map.Standard as Map

-- DATA DEFINITION
-------------------------------------------------------------------------------

newtype Bag a = Bag (Map.Map a Integer)
	deriving (Eq, Ord, Show, Read)

-- EXPORTED
-------------------------------------------------------------------------------

empty :: Ord a => Bag a
empty = Bag Map.empty

addElement :: Ord a => a -> Bag a -> Bag a
addElement a (Bag m) = Bag $ DCMA.alter f a m where
	f (Nothing) = Just (toInteger 1)
	f (Just count) = Just (count + 1)

removeElement :: Ord a => a -> Bag a -> Bag a
removeElement a (Bag m) = Bag $ DCMA.alter f a m where
	f (Nothing) = Nothing
	f (Just 1) = Nothing
	f (Just count) = Just (count - 1)

containsElement :: Ord a => a -> Bag a -> Bool
containsElement a (Bag m) = DCMK.containsKey a m

getElementsCount :: Ord a => Bag a -> Integer
getElementsCount (Bag m) = DCMF.foldl' (+) 0 m

toList :: Ord a => Bag a -> [a]
toList (Bag m) = DCMF.foldlWithKey' (\ans element count -> ans ++ (replicate (fromInteger count) element)) [] m

-- TODO
fromList :: Ord a => [a] -> Bag a
fromList = DL.foldl' (flip addElement) empty

foldr :: Ord a => (a -> b -> b) -> b -> Bag a -> b
foldr f a (Bag m) = DCMF.foldrWithKey (\element count ans -> DL.foldr f ans (replicate (fromInteger count) element)) a m

foldl :: Ord a => (b -> a -> b) -> b -> Bag a -> b
foldl f a (Bag m) = DCMF.foldlWithKey (\ans element count -> DL.foldl f ans (replicate (fromInteger count) element)) a m

foldr' :: Ord a => (a -> b -> b) -> b -> Bag a -> b
foldr' f a (Bag m) = DCMF.foldrWithKey' (\element count ans -> DL.foldr f ans (replicate (fromInteger count) element)) a m

foldl' :: Ord a => (b -> a -> b) -> b -> Bag a -> b
foldl' f a (Bag m) = DCMF.foldlWithKey' (\ans element count -> DL.foldl' f ans (replicate (fromInteger count) element)) a m

-- INSTANCE
-------------------------------------------------------------------------------

instance Ord a => DC.Collection (Bag a) where
	type DC.Element (Bag a) = a
	addElement = addElement
	removeElement = removeElement
	containsElement = containsElement

instance Ord a => DCC.Cardinality (Bag a) where
	getElementsCount = getElementsCount

instance Ord a => DCI.Import (Bag a) where
	fromList = fromList

instance Ord a => DCE.Export (Bag a) where
	toList = toList

instance Ord a => DCF.Foldable (Bag a) where
	foldr = foldr
	foldl = foldl
	foldr' = foldr'
	foldl' = foldl'

instance Ord a => DCB.Bag (Bag a)

