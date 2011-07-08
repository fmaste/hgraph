-- Author: Federico Mastellone (fmaste@gmail.com)

-- Binary relation class.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Relation.Binary (
	BinaryRelation(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC
import qualified Data.Collection.Set as DCS

-- CLASS
-------------------------------------------------------------------------------

class (DC.Collection r, DCS.Set (DomainSet r), DCS.Set (CodomainSet r)) => BinaryRelation r where
	-- The Relation type families.
	type DomainSet r
	type CodomainSet r

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- Adds an element to the domain of the Relation.
	-- If this element already exists the original Relation is returned.
	addDomainElement :: DC.Element (DomainSet r) -> r -> r

	-- Adds an element to the codomain of the Relation.
	-- If this element already exists the original Relation is returned.
	addCodomainElement :: DC.Element (CodomainSet r) -> r -> r

	-- Removes an element from the domain of the Relation.
	-- If this element does not exists the original Relation is returned.
	removeDomainElement :: DC.Element(DomainSet r) -> r -> r

	-- Removes an element from the codomain of the Relation.
	-- If this element does not exists the original Relation is returned.
	removeCodomainElement :: DC.Element (CodomainSet r) -> r -> r

	-- Adds a relation from a domain element to a codomain one.
	-- If the domain or codomain element is not present they are added.
	-- If this relation already exists for this elements the original Relation is returned.
	addRelation :: DC.Element (DomainSet r) -> DC.Element (CodomainSet r) -> r -> r

	-- Removes a relation from a domain element to a codomain one.
	-- If this relation is not present the original Relation is returned.
	removeRelation :: DC.Element (DomainSet r) -> DC.Element (CodomainSet r) -> r -> r

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	getDomain :: r -> DomainSet r

	getCodomain :: r -> CodomainSet r

	getRelatedTo :: DC.Element (DomainSet r) -> r -> CodomainSet r

	getRelatedFrom :: DC.Element (CodomainSet r) -> r -> DomainSet r

	-- All the relationships. Elements without relationships are not shown.
	-- This function can be constructed using other funtions, but it is
	-- here because the graph is part of the signature of a relation.
	-- getGraph :: r -> [(Domain r, Codomain r)]

-- TODO
-------------------------------------------------------------------------------

{-- Later on an extra class, Like RelationTheory, RelationPlus, etc.
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
--}

