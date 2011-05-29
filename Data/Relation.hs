-- Author: Federico Mastellone (fmaste@gmail.com)

-- Relation class.
-- TODO: Make it haddock compatibele!
{-# LANGUAGE TypeFamilies #-}
module Data.Relation (
	-- Atomic constructor functions.
	Relation(),
	empty,
	addDomainElement,
	addCodomainElement,
	removeDomainElement,
	removeCodomainElement,
	addRelation,
	removeRelation,
	-- Atomic query functions.
        getDomain,
	getCodomain,
	getRelatedTo,
	getRelatedFrom,
	getGraph,
	-- Util query functions.
	getDomainList,
	getCodomainList,
	getDomainCount,
	getCodomainCount,
	containsDomainElement,
	containsCodomainElement,
	getRelatedToList,
	getRelatedFromList,
	getRelatedToCount,
	getRelatedFromCount,
	isRelatedTo,
	isRelatedFrom,
	containsRelation,
	-- Relation theory functions.
	isInjective,
	revert) where

-- IMPORTS
-------------------------------------------------------------------------------

import Data.List (foldl, foldl', foldr)
import qualified Data.Set as Set

class Relation r where
	-- The domain and codomain type families.
	type Domain r
	type Codomain r

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- The empty relation.
	empty :: r

	-- Adds an element to the domain.
	-- If this element already exists the original Relation is returned.
	addDomainElement :: Domain r -> r -> r

	-- Adds an element to the codomain.
	-- If this element already exists the original Relation is returned.
	addCodomainElement :: Codomain r -> r -> r

	-- Removes an element from the domain.
	-- If this element does not exists the original Relation is returned.
	removeDomainElement ::Domain r -> r -> r

	-- Removes an element from the codomain.
	-- If this element does not exists the original Relation is returned.
	removeCodomainElement :: Codomain r -> r -> r

	-- Adds a relation from a domain element to a codomain one.
	-- If the domain or codomain element is not present they are added.
	-- If this relation already exists for this elements the original Relation is returned.
	addRelation :: Domain r -> Codomain r -> r -> r

	-- Removes a relation from a domain element to a codomain one.
	-- If this relation is not present the original BinaryRelation is returned.
	removeRelation :: Domain r -> Codomain r -> r -> r

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	getDomain :: r -> Set.Set (Domain r)

	getCodomain :: r -> Set.Set (Codomain r)

	getRelatedTo :: Domain r -> r -> Set.Set (Codomain r)

	getRelatedFrom :: Codomain r -> r -> Set.Set (Domain r)

	-- All the relationships. Elements without relationships are not shown.
	-- This function can be constructed using other funtions, but it is
	-- here because the graph is part of the signature of a relation.
	getGraph :: r -> Set.Set (Domain r, Codomain r)

	-- UTIL QUERY FUNCTIONS
	-----------------------------------------------------------------------

	getDomainList :: r -> [Domain r]
	getDomainList r = Set.elems $ getDomain r

	getCodomainList :: r -> [Codomain r]
	getCodomainList r = Set.elems $ getCodomain r

	getDomainCount :: r -> Int
	getDomainCount r = Set.size $ getDomain r

	getCodomainCount :: r -> Int
	getCodomainCount r = Set.size $ getCodomain r
	
	-- TODO: Need a generic Set (GSet) to make an implemenation here because of the ord restriction.
	containsDomainElement :: Domain r -> r -> Bool

	-- TODO: Need a generic Set (GSet) to make an implemenation here because of the ord restriction.
	containsCodomainElement :: Codomain r -> r -> Bool

	getRelatedToList :: Domain r -> r -> [Codomain r]
	getRelatedToList element r = Set.elems $ getRelatedTo element r

	getRelatedFromList :: Codomain r -> r -> [Domain r]
	getRelatedFromList element r = Set.elems $ getRelatedFrom element r

	getRelatedToCount :: Domain r -> r -> Int
	getRelatedToCount element r = Set.size $ getRelatedTo element r

	getRelatedFromCount :: Codomain r -> r -> Int
	getRelatedFromCount element r = Set.size $ getRelatedFrom element r

	isRelatedTo :: Domain r -> Codomain r -> r -> Bool

	isRelatedFrom :: Codomain r -> Domain r -> r -> Bool

	containsRelation :: Domain r -> Codomain r -> r -> Bool

	-- RELATION THEORY
	-----------------------------------------------------------------------

	isInjective :: r -> Bool

	revert :: r -> r

