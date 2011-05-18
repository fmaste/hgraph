-- Author: Federico Mastellone (fmaste@gmail.com)

-- Generic module to manage binary relationships.
-- TODO: Make it haddock compatibele!
module Data.Relation.Symmetric (
	-- Atomic constructor functions.
	BinaryRelation(),
	empty,
	addElement,
	removeElement,
	addRelation,
	removeRelation,
	-- Atomic query functions.
	getDomain,
	getRelatedTo,
	getGraph,
	-- Util query functions.
	getDomainList,
	getDomainCount,
	containsDomainElement,
	getRelatedToList,
	getRelatedToCount,
	isRelatedTo,
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

-- We defined BinaryRelation with a MultiMap structure.
newtype BinaryRelation domain = BinaryRelation (MM.MultiMap domain domain)
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

-- Removes an element from the domain.
-- If this element does not exists the original BinaryRelation is returned.
removeDomainElement :: Ord domain => domain -> BinaryRelation domain -> BinaryRelation domain
removeDomainElement domain (BinaryRelation relatedTo) = BinaryRelation relatedTo' where
	relatedTo' = MM.removeValue domain relatedTo

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

getRelatedTo :: Ord domain => domain -> BinaryRelation domain -> Set.Set codomain
getRelatedTo element (BinaryRelation relatedTo _) = MM.getValues element relatedTo

-- All the relationships. Elements without relationships are not shown.
-- This function can be constructed using other funtions, but it is
-- here because the graph is part of the signature of a binary relation.
getGraph :: Ord domain => BinaryRelation domain -> Set.Set (domain, codomain)
-- TODO: Make it more performant, it is traversing the sets too many times.
getGraph br = Set.fromList [ (domain, codomain) | domain <- getDomainList br, codomain <- getRelatedToList domain br]

-- UTIL QUERY FUNCTIONS
-------------------------------------------------------------------------------

getDomainList :: Ord domain => BinaryRelation domain -> [domain]
getDomainList (BinaryRelation relatedTo _) = MM.getKeys relatedTo

getDomainCount :: Ord domain => BinaryRelation domain -> Int
getDomainCount (BinaryRelation relatedTo _) = MM.getKeyCount relatedTo

containsDomainElement :: Ord domain => domain -> BinaryRelation domain -> Bool
containsDomainElement element (BinaryRelation relatedTo _) = MM.containsKey element relatedTo

getRelatedToList :: Ord domain => domain -> BinaryRelation domain -> [codomain]
getRelatedToList element (BinaryRelation relatedTo _) = MM.getValuesList element relatedTo

getRelatedToCount :: Ord domain => domain -> BinaryRelation domain -> Int
getRelatedToCount element (BinaryRelation relatedTo _) = MM.getValueCount element relatedTo

isRelatedTo :: Ord domain => domain -> codomain -> BinaryRelation domain -> Bool
isRelatedTo domain codomain br = containsRelation domain codomain br

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

