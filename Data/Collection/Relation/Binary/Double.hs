-- Author: Federico Mastellone (fmaste@gmail.com)

-- Generic module to manage binary relationships.
-- TODO: Make it haddock compatibele!

-- MODULE
-------------------------------------------------------------------------------

module Data.Collection.Relation.Binary.Double (
	-- Atomic constructor functions.
	BinaryRelation(),
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
import qualified Data.MultiMap as MM

-- DATA DEFINITION
-------------------------------------------------------------------------------

-- We defined BinaryRelation with two structures, one with the domain -> codomain 
-- relationships and the other with the codomain <- domain ones.
-- This double structure helps to make faster queries but less performant delete operations.
data BinaryRelation domain codomain = BinaryRelation (RelatedTo domain codomain) (RelatedFrom domain codomain)
    deriving (Show, Read, Ord, Eq)

-- A domain element contains a Set of codomain elements.
type RelatedTo domain codomain = MM.MultiMap domain codomain

-- A codomain element contains a Set of domain elements.
type RelatedFrom domain codomain = MM.MultiMap codomain domain

-- ATOMIC CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- The empty binary relation.
empty :: (Ord domain, Ord codomain) => BinaryRelation domain codomain
empty = BinaryRelation MM.empty MM.empty

-- Adds an element to the domain.
-- If this element already exists the original BinaryRelation is returned.
addDomainElement :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
addDomainElement element (BinaryRelation relatedTo relatedFrom) = BinaryRelation relatedTo' relatedFrom where
	relatedTo' = MM.addKey element relatedTo

-- Adds an element to the codomain.
-- If this element already exists the original BinaryRelation is returned.
addCodomainElement :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
addCodomainElement element (BinaryRelation relatedTo relatedFrom) = BinaryRelation relatedTo relatedFrom' where
	relatedFrom' = MM.addKey element relatedFrom

-- Removes an element from the domain.
-- If this element does not exists the original BinaryRelation is returned.
removeDomainElement :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
removeDomainElement domain (BinaryRelation relatedTo relatedFrom) = 
	let
		(relatedTo', relatedToElements) = MM.getValuesAndRemoveKey domain relatedTo
		relatedFrom' = MM.removeValueFromKeys relatedToElements domain relatedFrom
	in BinaryRelation relatedTo' relatedFrom'

-- Removes an element from the codomain.
-- If this element does not exists the original BinaryRelation is returned.
removeCodomainElement :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
removeCodomainElement codomain (BinaryRelation relatedTo relatedFrom) = 
	let
		(relatedFrom', relatedFromElements) = MM.getValuesAndRemoveKey codomain relatedFrom
		relatedTo' = MM.removeValueFromKeys relatedFromElements codomain relatedTo
	in BinaryRelation relatedTo' relatedFrom'

-- Adds a relation from a domain element to a codomain one.
-- If the domain or codomain element is not present they are added.
-- If this relation already exists for this elements the original BinaryRelation is returned.
addRelation :: (Ord domain, Ord codomain) => domain -> codomain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
addRelation domain codomain (BinaryRelation relatedTo relatedFrom) =
	let 
		relatedTo' = MM.addValue domain codomain relatedTo
		relatedFrom' = MM.addValue codomain domain relatedFrom
	in BinaryRelation relatedTo' relatedFrom'

-- Removes a relation from a domain element to a codomain one.
-- If this relation is not present the original BinaryRelation is returned.
removeRelation :: (Ord domain, Ord codomain) => domain -> codomain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
removeRelation domain codomain (BinaryRelation relatedTo relatedFrom) = 
	let 
		relatedTo' = MM.removeValue domain codomain relatedTo
		relatedFrom' = MM.removeValue codomain domain relatedFrom
	in BinaryRelation relatedTo' relatedFrom'

-- ATOMIC QUERY FUNCTIONS
-------------------------------------------------------------------------------

getDomain :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Set.Set domain
getDomain (BinaryRelation relatedTo _) = MM.getKeysSet relatedTo

getCodomain :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Set.Set codomain
getCodomain (BinaryRelation _ relatedFrom) = MM.getKeysSet relatedFrom

getRelatedTo :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> Set.Set codomain
getRelatedTo element (BinaryRelation relatedTo _) = MM.getValues element relatedTo

getRelatedFrom :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> Set.Set domain
getRelatedFrom element (BinaryRelation _ relatedFrom) = MM.getValues element relatedFrom

-- All the relationships. Elements without relationships are not shown.
-- This function can be constructed using other funtions, but it is
-- here because the graph is part of the signature of a binary relation.
getGraph :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Set.Set (domain, codomain)
-- TODO: Make it more performant, it is traversing the sets too many times.
getGraph br = Set.fromList [ (domain, codomain) | domain <- getDomainList br, codomain <- getRelatedToList domain br]

-- UTIL QUERY FUNCTIONS
-------------------------------------------------------------------------------

getDomainList :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> [domain]
getDomainList (BinaryRelation relatedTo _) = MM.getKeys relatedTo

getCodomainList :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> [codomain]
getCodomainList (BinaryRelation _ relatedFrom) = MM.getKeys relatedFrom

getDomainCount :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Int
getDomainCount (BinaryRelation relatedTo _) = MM.getKeyCount relatedTo

getCodomainCount :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Int
getCodomainCount (BinaryRelation _ relatedFrom) = MM.getKeyCount relatedFrom

containsDomainElement :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> Bool
containsDomainElement element (BinaryRelation relatedTo _) = MM.containsKey element relatedTo

containsCodomainElement :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> Bool
containsCodomainElement element (BinaryRelation _ relatedFrom) = MM.containsKey element relatedFrom

getRelatedToList :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> [codomain]
getRelatedToList element (BinaryRelation relatedTo _) = MM.getValuesList element relatedTo

getRelatedFromList :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> [domain]
getRelatedFromList element (BinaryRelation _ relatedFrom) = MM.getValuesList element relatedFrom

getRelatedToCount :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> Int
getRelatedToCount element (BinaryRelation relatedTo _) = MM.getValueCount element relatedTo

getRelatedFromCount :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> Int
getRelatedFromCount element (BinaryRelation _ relatedFrom) = MM.getValueCount element relatedFrom

isRelatedTo :: (Ord domain, Ord codomain) => domain -> codomain -> BinaryRelation domain codomain -> Bool
isRelatedTo domain codomain br = containsRelation domain codomain br

isRelatedFrom :: (Ord domain, Ord codomain) => codomain -> domain -> BinaryRelation domain codomain -> Bool
isRelatedFrom codomain domain br = containsRelation domain codomain br

containsRelation :: (Ord domain, Ord codomain) => domain -> codomain ->  BinaryRelation domain codomain -> Bool
containsRelation domain codomain  (BinaryRelation relatedTo _) = MM.containsValue domain codomain relatedTo

-- RELATION THEORY
-------------------------------------------------------------------------------

isInjective :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Bool
isInjective br = all (\codomain -> getRelatedFromCount codomain br <= 1) $ getCodomainList br

revert :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> BinaryRelation codomain domain
revert (BinaryRelation relatedTo relatedFrom) = BinaryRelation relatedFrom relatedTo

--TODO: Add more functions, like image, range, isFunction, biyective, etc, etc.

-- TEST
-------------------------------------------------------------------------------

testWithFoldl' n =
	foldl' 
		(\ans (d,c) -> addRelation d c ans) 
		(foldl' 
			(\ans e -> addCodomainElement e ans) 
			(foldl' 
				(\ans e -> addDomainElement e ans) 
				empty 
				[1..n]
			) 
			[1..n]
		) 
		[ (d,c) | d <- [1..n], c <- [1..n]]

testWithLetFoldl' n =
	let
		domainAdded = foldl' (\ans e -> addDomainElement e ans) empty [1..n]
		domainAndCodomainAdded = foldl' (\ans e -> addCodomainElement e ans) domainAdded [1..n]
	in foldl' (\ans (d,c) -> addRelation d c ans) domainAndCodomainAdded [ (d,c) | d <- [1..n], c <- [1..n]]

testWithFoldr n =
        foldr 
                (\(d,c) ans -> addRelation d c ans)
                (foldr
                        (\e ans -> addCodomainElement e ans)
                        (foldr
                                (\e ans -> addDomainElement e ans)
                                empty
                                [1..n]
                        )
                        [1..n]
                )
                [ (d,c) | d <- [1..n], c <- [1..n]]

testWithLetFoldr n =
        let
                domainAdded = foldr (\e ans -> addDomainElement e ans) empty [1..n]
                domainAndCodomainAdded = foldr (\e ans-> addCodomainElement e ans) domainAdded [1..n]
        in foldr (\(d,c) ans-> addRelation d c ans) domainAndCodomainAdded [ (d,c) | d <- [1..n], c <- [1..n]]

