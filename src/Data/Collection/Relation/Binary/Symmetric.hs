-- Author: Federico Mastellone (fmaste@gmail.com)

-- Generic module to manage binary relationships.
-- TODO: Make it haddock compatibele!
{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Relation.Binary.Symmetric (
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
import qualified Data.Collection.List as DCL
import qualified Data.Collection.Map.Multi.Set.Standard as MM

-- DATA DEFINITION
-------------------------------------------------------------------------------

-- We defined BinaryRelation with a MapSet structure.
newtype BinaryRelation domain = BinaryRelation (MM.MapSet domain domain)
    deriving (Show, Read, Ord, Eq)

-- ATOMIC CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- The empty binary relation.
empty :: Ord domain => BinaryRelation domain
empty = BinaryRelation MM.empty

-- Adds an element to the domain.
-- If this element already exists the original BinaryRelation is returned.
addDomainElement :: Ord domain => domain -> BinaryRelation domain -> BinaryRelation domain
addDomainElement element (BinaryRelation relatedTo) = BinaryRelation relatedTo' where
	relatedTo' = MM.addKey element relatedTo

-- Adds an element to the codomain.
-- If this element already exists the original BinaryRelation is returned.
addCodomainElement :: Ord domain => domain -> BinaryRelation domain -> BinaryRelation domain
addCodomainElement element br = addDomainElement element bt

-- Removes an element from the domain.
-- If this element does not exists the original BinaryRelation is returned.
removeDomainElement :: Ord domain => domain -> BinaryRelation domain -> BinaryRelation domain
removeDomainElement domain (BinaryRelation relatedTo) = BinaryRelation relatedTo' where
	relatedTo' = MM.removeKey domain relatedTo

-- Removes an element from the codomain.
-- If this element does not exists the original BinaryRelation is returned.
removeCodomainElement :: Ord domain => domain -> BinaryRelation domain -> BinaryRelation domain
removeCodomainElement domain br = removeDomainElement domain br

-- Adds a relation from a domain element to a codomain one.
-- If the domain or codomain element is not present they are added.
-- If this relation already exists for this elements the original BinaryRelation is returned.
addRelation :: Ord domain => domain -> codomain -> BinaryRelation domain -> BinaryRelation domain
addRelation domain codomain (BinaryRelation relatedTo relatedFrom) =
	let 
		relatedTo' = MM.addValue domain codomain relatedTo
		relatedFrom' = MM.addValue codomain domain relatedFrom
	in BinaryRelation relatedTo' relatedFrom'

-- Removes a relation from a domain element to a codomain one.
-- If this relation is not present the original BinaryRelation is returned.
removeRelation :: Ord domain => domain -> codomain -> BinaryRelation domain -> BinaryRelation domain
removeRelation domain codomain (BinaryRelation relatedTo relatedFrom) = 
	let 
		relatedTo' = MM.removeValue domain codomain relatedTo
		relatedFrom' = MM.removeValue codomain domain relatedFrom
	in BinaryRelation relatedTo' relatedFrom'

-- ATOMIC QUERY FUNCTIONS
-------------------------------------------------------------------------------

getDomain :: Ord domain => BinaryRelation domain -> Set.Set domain
getDomain (BinaryRelation relatedTo _) = MM.getKeysSet relatedTo

getCodomain :: Ord domain => BinaryRelation domain -> Set.Set domain
getCodomain br = getDomain br

getRelatedTo :: Ord domain => domain -> BinaryRelation domain -> Set.Set codomain
getRelatedTo element (BinaryRelation relatedTo _) = MM.getValues element relatedTo

getRelatedFrom :: Ord domain => domain -> BinaryRelation domain -> Set.Set codomain
getRelatedFrom element br = getRelatedTo br

-- All the relationships. Elements without relationships are not shown.
-- This function can be constructed using other funtions, but it is
-- here because the graph is part of the signature of a binary relation.
getGraph :: Ord domain => BinaryRelation domain -> Set.Set (domain, codomain)
-- TODO: Make it more performant, it is traversing the sets too many times.
getGraph br = DCL.fromList [ (domain, codomain) | domain <- getDomainList br, codomain <- getRelatedToList domain br]

-- UTIL QUERY FUNCTIONS
-------------------------------------------------------------------------------

getDomainList :: Ord domain => BinaryRelation domain -> [domain]
getDomainList (BinaryRelation relatedTo _) = MM.getKeys relatedTo


getDomainList :: Ord domain => BinaryRelation domain -> [domain]
getDomainList (BinaryRelation relatedTo _) = MM.getKeys relatedTo

getDomainCount :: Ord domain => BinaryRelation domain -> Int
getDomainCount (BinaryRelation relatedTo _) = MM.getKeyCount relatedTo

getCodomainCount :: Ord domain => BinaryRelation domain -> Int 
getCodomainCount br = getDomainCount br

containsDomainElement :: Ord domain => domain -> BinaryRelation domain -> Bool
containsDomainElement element (BinaryRelation relatedTo _) = MM.containsKey element relatedTo

containsCodomainElement :: Ord domain => domain -> BinaryRelation domain -> Bool
containsCodomainElement element br = containsDomainElement element br

getRelatedToList :: Ord domain => domain -> BinaryRelation domain -> [codomain]
getRelatedToList element (BinaryRelation relatedTo _) = MM.getValuesList element relatedTo

getRelatedFromList :: Ord domain => domain -> BinaryRelation domain -> [codomain]
getRelatedFromList element br = getRelatedToList element br

getRelatedToCount :: Ord domain => domain -> BinaryRelation domain -> Int
getRelatedToCount element (BinaryRelation relatedTo _) = MM.getValueCount element relatedTo

getRelatedFromCount :: Ord domain => domain -> BinaryRelation domain -> Int 
getRelatedFromCount element br = getRelatedToCount element br

isRelatedTo :: Ord domain => domain -> codomain -> BinaryRelation domain -> Bool
isRelatedTo domain codomain br = containsRelation domain codomain br

isRelatedFrom :: Ord domain => domain -> codomain -> BinaryRelation domain -> Bool
isRelatedFrom domain codomain br = containsRelation codomain domain br

containsRelation :: Ord domain => domain -> codomain ->  BinaryRelation domain -> Bool
containsRelation domain codomain  (BinaryRelation relatedTo _) = MM.containsValue domain codomain relatedTo

-- RELATION THEORY
-------------------------------------------------------------------------------

isInjective :: Ord domain => BinaryRelation domain -> Bool
isInjective br = all (\codomain -> getRelatedFromCount codomain br <= 1) $ getCodomainList br

revert :: Ord domain => BinaryRelation domain -> BinaryRelation codomain domain
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

