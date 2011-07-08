-- Author: Federico Mastellone (fmaste@gmail.com)

-- Generic module to manage binary relationships.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Relation.Binary.Double (
	BinaryRelation(),
	-- Atomic constructor functions.
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
	containsRelation,
	getGraph,
	-- Util domain query functions.
	getDomainList,
	getDomainCount,
	containsDomainElement,
	getRelatedToList,
	getRelatedToCount,
	isRelatedTo,
	-- Util codomain query functions.
	getCodomainList,
	getCodomainCount,
	containsCodomainElement,
	getRelatedFromList,
	getRelatedFromCount,
	isRelatedFrom,
	-- Theory functions.
	isInjective,
	-- Transformation functions.
	revert) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC
import qualified Data.Collection.Cardinality as DCC
import qualified Data.Collection.Import as DCI
import qualified Data.Collection.Export as DCE
import qualified Data.Collection.Map as DCM
import qualified Data.Collection.Map.Keys as DCMK
import qualified Data.Collection.Map.Multi as DCMM
import qualified Data.Collection.Relation.Binary as DCRB
import qualified Data.Collection.Relation.Binary.Domain as DCRBD
import qualified Data.Collection.Relation.Binary.Codomain as DCRBC
import qualified Data.Collection.Map.Multi.Set.Standard as MapSet
import qualified Data.Collection.Set.Standard as Set

-- DATA DEFINITION
-------------------------------------------------------------------------------

-- We defined BinaryRelation with two structures, one with the domain -> codomain 
-- relationships and the other with the codomain <- domain ones.
-- This double structure helps to make faster queries but less performant delete operations.
data BinaryRelation domain codomain = BinaryRelation (RelatedTo domain codomain) (RelatedFrom domain codomain)
    deriving (Show, Read, Ord, Eq)

-- A domain element contains a Set of codomain elements.
type RelatedTo domain codomain = MapSet.MapSet domain codomain

-- A codomain element contains a Set of domain elements.
type RelatedFrom domain codomain = MapSet.MapSet codomain domain

-- EXPORTED
-------------------------------------------------------------------------------

-- ATOMIC CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- The empty binary relation.
empty :: (Ord domain, Ord codomain) => BinaryRelation domain codomain
empty = BinaryRelation MapSet.empty MapSet.empty

-- Adds an element to the domain.
-- If this element already exists the original BinaryRelation is returned.
addDomainElement :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
addDomainElement element (BinaryRelation relatedTo relatedFrom) = BinaryRelation relatedTo' relatedFrom where
	relatedTo' = DCMM.addKey element relatedTo

-- Adds an element to the codomain.
-- If this element already exists the original BinaryRelation is returned.
addCodomainElement :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
addCodomainElement element (BinaryRelation relatedTo relatedFrom) = BinaryRelation relatedTo relatedFrom' where
	relatedFrom' = DCMM.addKey element relatedFrom

-- Removes an element from the domain.
-- If this element does not exists the original BinaryRelation is returned.
removeDomainElement :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
removeDomainElement domain (BinaryRelation relatedTo relatedFrom) = 
	let
		(relatedTo', relatedToElements) = f $ DCM.getValueAndRemoveKey domain relatedTo where
			f (Just relatedToElements, relatedTo) = (relatedTo, relatedToElements)
			f (Nothing, relatedTo) = (relatedTo, Set.empty)
		relatedFrom' = DCMM.removeFromKeys (DCE.toList relatedToElements) domain relatedFrom
	in BinaryRelation relatedTo' relatedFrom'

-- Removes an element from the codomain.
-- If this element does not exists the original BinaryRelation is returned.
removeCodomainElement :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
removeCodomainElement codomain (BinaryRelation relatedTo relatedFrom) = 
	let
		(relatedFrom', relatedFromElements) = f $ DCM.getValueAndRemoveKey codomain relatedFrom where
			f (Just relatedFromElements, relatedFrom) = (relatedFrom, relatedFromElements)
			f (Nothing, relatedFrom) = (relatedFrom, Set.empty)
		relatedTo' = DCMM.removeFromKeys (DCE.toList relatedFromElements) codomain relatedTo
	in BinaryRelation relatedTo' relatedFrom'

-- Adds a relation from a domain element to a codomain one.
-- If the domain or codomain element is not present they are added.
-- If this relation already exists for this elements the original BinaryRelation is returned.
addRelation :: (Ord domain, Ord codomain) => domain -> codomain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
addRelation domain codomain (BinaryRelation relatedTo relatedFrom) =
	let 
		relatedTo' = DCMM.addToKey domain codomain relatedTo
		relatedFrom' = DCMM.addToKey codomain domain relatedFrom
	in BinaryRelation relatedTo' relatedFrom'

-- Removes a relation from a domain element to a codomain one.
-- If this relation is not present the original BinaryRelation is returned.
removeRelation :: (Ord domain, Ord codomain) => domain -> codomain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
removeRelation domain codomain (BinaryRelation relatedTo relatedFrom) = 
	let 
		relatedTo' = DCMM.removeFromKey domain codomain relatedTo
		relatedFrom' = DCMM.removeFromKey codomain domain relatedFrom
	in BinaryRelation relatedTo' relatedFrom'

-- ATOMIC QUERY FUNCTIONS
-------------------------------------------------------------------------------

getDomain :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Set.Set domain
getDomain (BinaryRelation relatedTo _) = Set.fromList $ DCM.getKeys relatedTo

getCodomain :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Set.Set codomain
getCodomain (BinaryRelation _ relatedFrom) = Set.fromList $ DCM.getKeys relatedFrom

getRelatedTo :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> Set.Set codomain
getRelatedTo element (BinaryRelation relatedTo _) = DCM.getValueWithDefault Set.empty element relatedTo

getRelatedFrom :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> Set.Set domain
getRelatedFrom element (BinaryRelation _ relatedFrom) = DCM.getValueWithDefault Set.empty element relatedFrom

containsRelation :: (Ord domain, Ord codomain) => domain -> codomain ->  BinaryRelation domain codomain -> Bool
containsRelation domain codomain  (BinaryRelation relatedTo _) = DCMM.containedInKey domain codomain relatedTo

getGraph :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> [(domain, codomain)]
-- TODO: Make it more performant, it is traversing the sets too many times.
getGraph br = [ (domain, codomain) | domain <- getDomainList br, codomain <- getRelatedToList domain br]

-- UTIL DOMAIN QUERY FUNCTIONS
-------------------------------------------------------------------------------

getDomainList :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> [domain]
getDomainList (BinaryRelation relatedTo _) = DCM.getKeys relatedTo

getDomainCount :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Int
getDomainCount (BinaryRelation relatedTo _) = fromInteger $ DCMK.getKeysCount relatedTo

containsDomainElement :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> Bool
containsDomainElement element (BinaryRelation relatedTo _) = DCMK.containsKey element relatedTo

getRelatedToList :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> [codomain]
getRelatedToList element br = DCE.toList $ getRelatedTo element br

getRelatedToCount :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> Int
getRelatedToCount element (BinaryRelation relatedTo _) = fromInteger $ DCMM.getValuesCount element relatedTo

isRelatedTo :: (Ord domain, Ord codomain) => domain -> codomain -> BinaryRelation domain codomain -> Bool
isRelatedTo domain codomain br = containsRelation domain codomain br

-- UTIL CODOMAIN QUERY FUNCTIONS
-------------------------------------------------------------------------------

getCodomainList :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> [codomain]
getCodomainList (BinaryRelation _ relatedFrom) = DCM.getKeys relatedFrom

getCodomainCount :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Int
getCodomainCount (BinaryRelation _ relatedFrom) = fromInteger $ DCMK.getKeysCount relatedFrom

containsCodomainElement :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> Bool
containsCodomainElement element (BinaryRelation _ relatedFrom) = DCMK.containsKey element relatedFrom

getRelatedFromList :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> [domain]
getRelatedFromList element br = DCE.toList $ getRelatedFrom element br

getRelatedFromCount :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> Int
getRelatedFromCount element (BinaryRelation _ relatedFrom) = fromInteger $ DCMM.getValuesCount element relatedFrom

isRelatedFrom :: (Ord domain, Ord codomain) => codomain -> domain -> BinaryRelation domain codomain -> Bool
isRelatedFrom codomain domain br = containsRelation domain codomain br

-- RELATION THEORY
-------------------------------------------------------------------------------

isInjective :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Bool
isInjective br = all (\codomain -> getRelatedFromCount codomain br <= 1) $ getCodomainList br

revert :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> BinaryRelation codomain domain
revert (BinaryRelation relatedTo relatedFrom) = BinaryRelation relatedFrom relatedTo

--TODO: Add more functions, like image, range, isFunction, biyective, etc, etc.

-- INSTANCES
-------------------------------------------------------------------------------

instance (Ord domain, Ord codomain) => DC.Collection (BinaryRelation domain codomain) where
	type DC.Element (BinaryRelation domain codomain) = (domain, codomain)
	addElement (d, c) br = addRelation d c br
	removeElement (d, c) br = removeRelation d c br
	containsElement (d, c) br = containsRelation d c br

instance (Ord domain, Ord codomain) => DCC.Cardinality (BinaryRelation domain codomain) where
	getElementsCount br = toInteger $ length $ getGraph br

instance (Ord domain, Ord codomain) => DCRB.BinaryRelation (BinaryRelation domain codomain) where
	type DCRB.DomainSet (BinaryRelation domain codomain) = Set.Set domain
	type DCRB.CodomainSet (BinaryRelation domain codomain) = Set.Set codomain
	addDomainElement = addDomainElement
	addCodomainElement = addCodomainElement
	removeDomainElement = removeDomainElement
	removeCodomainElement = removeCodomainElement
	addRelation = addRelation
	removeRelation = removeRelation
	getDomain = getDomain
	getCodomain = getCodomain
	getRelatedTo = getRelatedTo
	getRelatedFrom = getRelatedFrom
	containsRelation = containsRelation
	getGraph = getGraph

instance (Ord domain, Ord codomain) => DCRBD.Domain (BinaryRelation domain codomain) where
	getDomainList = getDomainList
	-- TODO: Remove toInteger
	getDomainCount r = toInteger $ getDomainCount r
	containsDomainElement = containsDomainElement
	getRelatedToList = getRelatedToList
	-- TODO: Remove toInteger
	getRelatedToCount d r = toInteger $ getRelatedToCount d r
	isRelatedTo = isRelatedTo

instance (Ord domain, Ord codomain) => DCRBC.Codomain (BinaryRelation domain codomain) where
	getCodomainList = getCodomainList
	-- TODO: Remove toInteger
	getCodomainCount r = toInteger $ getCodomainCount r
	containsCodomainElement = containsCodomainElement
	getRelatedFromList = getRelatedFromList
	-- TODO: Remove toInteger
	getRelatedFromCount c r = toInteger $ getRelatedFromCount c r
	isRelatedFrom = isRelatedFrom

