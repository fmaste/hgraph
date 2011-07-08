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

	containsRelation :: DC.Element (DomainSet r) -> DC.Element (CodomainSet r) ->  r -> Bool

	-- TODO: Add getRange function

	-- TODO: add getImage function

	-- All the relationships. Elements without relationships are not shown.
	-- This function can be constructed using other funtions, but it is
	-- here because the graph is part of the signature of a relation.
	-- TODO: Return a Set of (domain, codomain).
	getGraph :: r -> [(DC.Element (DomainSet r), DC.Element (CodomainSet r))]

