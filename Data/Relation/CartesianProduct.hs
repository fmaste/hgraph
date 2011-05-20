-- Author: Federico Mastellone (fmaste@gmail.com)

-- Generic AxB cartesian product module.
-- http://mathworld.wolfram.com/CartesianProduct.html
module Data.Relation.CartesianProduct (
	-- Atomic constructor functions.
	CartesianProduct(),
	empty,
	addAElement,
	addBElement,
	removeAElement,
	removeBElement,
	-- Atomic query functions.
	getA,
	getB,
	containsAElement,
	containsBElement,
	getPoints,
	containsPoint ) where 

-- IMPORTS
-------------------------------------------------------------------------------

import Data.List (foldl, foldl', foldr)
import qualified Data.Set as Set
import qualified Data.MultiMap as MM

-- DATA DEFINITION
-------------------------------------------------------------------------------

-- The AxB cartesian product. Contains a set of As and a set of Bs.
data CartesianProduct a b = CartesianProduct (Set.Set a) (Set.Set b)
	deriving (Show, Read, Ord, Eq)

-- ATOMIC CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- The empty AxB cartesian product.
empty :: (Ord a, Ord b) => CartesianProduct a b
empty = CartesianProduct Set.empty Set.empty

-- Adds an element to the A set.
-- If this element already exists the original CartesianProduct is returned.
addAElement :: (Ord a, Ord b) => a -> CartesianProduct a b -> CartesianProduct a b
addAElement a (CartesianProduct as bs) = CartesianProduct (Set.insert a as) bs

-- Adds an element to the B set.
-- If this element already exists the original CartesianProduct is returned.
addBElement :: (Ord a, Ord b) => b -> CartesianProduct a b -> CartesianProduct a b
addBElement b (CartesianProduct as bs) = CartesianProduct as (Set.insert b bs)

-- Removes an element to the A set.
-- If this element does not exists the original CartesianProduct is returned.
removeAElement :: (Ord a, Ord b) => a -> CartesianProduct a b -> CartesianProduct a b
removeAElement a (CartesianProduct as bs) = CartesianProduct (Set.delete a as) bs

-- Removes an element to the B set.
-- If this element does not exists the original CartesianProduct is returned.
removeBElement :: (Ord a, Ord b) => b -> CartesianProduct a b -> CartesianProduct a b
removeBElement b (CartesianProduct as bs) = CartesianProduct as (Set.delete b bs)

-- ATOMIC QUERY FUNCTIONS
-------------------------------------------------------------------------------

getA :: (Ord a, Ord b) => CartesianProduct a b -> Set.Set a
getA (CartesianProduct as _) = as

getB :: (Ord a, Ord b) => CartesianProduct a b -> Set.Set b
getB (CartesianProduct _ bs) = bs

containsAElement :: (Ord a, Ord b) => a -> CartesianProduct a b -> Bool
containsAElement a (CartesianProduct as _) = Set.member a as

containsBElement :: (Ord a, Ord b) => b -> CartesianProduct a b -> Bool
containsBElement b (CartesianProduct _ bs) = Set.member b bs

getPoints :: (Ord a, Ord b) => CartesianProduct a b -> Set.Set (a, b)
getPoints cp = Set.unions $ map f (Set.elems $ getB cp) where
	f b = Set.map (\a -> (a,b)) $ getA cp

containsPoint :: (Ord a, Ord b) => (a, b) -> CartesianProduct a b -> Bool
containsPoint (a,b) (CartesianProduct as bs) = (Set.member a as) && (Set.member b bs)

