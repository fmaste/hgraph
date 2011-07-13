-- Author: Federico Mastellone (fmaste@gmail.com)

-- Int bag.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Bag.Int (
	IntBag,
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
import qualified Data.Collection.Map.Int as IntMap

-- DATA DEFINITION
-------------------------------------------------------------------------------

newtype IntBag = IntBag (IntMap.IntMap Integer)
	deriving (Eq, Ord, Show, Read)

-- EXPORTED
-------------------------------------------------------------------------------

empty :: IntBag
empty = IntBag IntMap.empty

addElement :: Int -> IntBag -> IntBag
addElement a (IntBag m) = IntBag $ DCMA.alter f a m where
	f (Nothing) = Just (toInteger 1)
	f (Just count) = Just (count + 1)

removeElement :: Int -> IntBag -> IntBag
removeElement a (IntBag m) = IntBag $ DCMA.alter f a m where
	f (Nothing) = Nothing
	f (Just 1) = Nothing
	f (Just count) = Just (count - 1)

containsElement :: Int -> IntBag -> Bool
containsElement a (IntBag m) = DCMK.containsKey a m

getElementsCount :: IntBag -> Integer
getElementsCount (IntBag m) = DCMF.foldl' (+) 0 m

toList :: IntBag -> [Int]
toList (IntBag m) = DCMF.foldlWithKey' (\ans element count -> ans ++ (replicate (fromInteger count) element)) [] m

-- TODO
fromList :: [Int] -> IntBag
fromList = DL.foldl' (flip addElement) empty

foldr :: (Int -> b -> b) -> b -> IntBag -> b
foldr f a (IntBag m) = DCMF.foldrWithKey (\element count ans -> DL.foldr f ans (replicate (fromInteger count) element)) a m

foldl :: (b -> Int -> b) -> b -> IntBag -> b
foldl f a (IntBag m) = DCMF.foldlWithKey (\ans element count -> DL.foldl f ans (replicate (fromInteger count) element)) a m

foldr' :: (Int -> b -> b) -> b -> IntBag -> b
foldr' f a (IntBag m) = DCMF.foldrWithKey' (\element count ans -> DL.foldr f ans (replicate (fromInteger count) element)) a m

foldl' :: (b -> Int -> b) -> b -> IntBag -> b
foldl' f a (IntBag m) = DCMF.foldlWithKey' (\ans element count -> DL.foldl' f ans (replicate (fromInteger count) element)) a m

-- INSTANCE
-------------------------------------------------------------------------------

instance DC.Collection IntBag where
	type DC.Element IntBag = Int
	addElement = addElement
	removeElement = removeElement
	containsElement = containsElement

instance DCC.Cardinality IntBag where
	getElementsCount = getElementsCount

instance DCI.Import IntBag where
	fromList = fromList

instance DCE.Export IntBag where
	toList = toList

instance DCF.Foldable IntBag where
	foldr = foldr
	foldl = foldl
	foldr' = foldr'
	foldl' = foldl'

instance DCB.Bag IntBag

