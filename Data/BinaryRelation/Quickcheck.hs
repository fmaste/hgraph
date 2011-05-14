-- Author: Federico Mastellone (fmaste@gmail.com)

module Data.BinaryRelation.Quickcheck (
	prop_addToDomain,
	prop_addToCodomain,
	prop_addToDomainCount,
	prop_addToCodomainCount,
	prop_addToDomainCheckCodomain,
	prop_addToCodomainCheckDomain,
	prop_addToDomainCheckCodomainCount,
	prop_addToCodomainCheckDomainCount) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.BinaryRelation as BR
import qualified Test.QuickCheck as QC

-- * UTILS
-------------------------------------------------------------------------------

-- | Add a list of elements to the domain of provided BinaryRelation.
addElementsToDomain :: (Ord domain, Ord codomain) => [domain] -> BR.BinaryRelation domain codomain -> BR.BinaryRelation domain codomain
addElementsToDomain elements br = List.foldl' (\br' element -> BR.addDomainElement element br') br elements

-- | Add a list of elements to the codomain of provided BinaryRelation.
addElementsToCodomain :: (Ord domain, Ord codomain) => [codomain] -> BR.BinaryRelation domain codomain -> BR.BinaryRelation domain codomain
addElementsToCodomain elements br = List.foldl' (\br' element -> BR.addCodomainElement element br') br elements

-- | Remove a list of elements from the domain of provided BinaryRelation.
removeElementsFromDomain :: (Ord domain, Ord codomain) => [domain] -> BR.BinaryRelation domain codomain -> BR.BinaryRelation domain codomain
removeElementsFromDomain elements br = List.foldl' (\br' element -> BR.removeDomainElement element br') br elements

-- | Remove a list of elements from the codomain of provided BinaryRelation.
removeElementsFromCodomain :: (Ord domain, Ord codomain) => [codomain] -> BR.BinaryRelation domain codomain -> BR.BinaryRelation domain codomain
removeElementsFromCodomain elements br = List.foldl' (\br' element -> BR.removeCodomainElement element br') br elements

-- | Add a list of relations to the provided BinaryRelation.
addRelations :: (Ord domain, Ord codomain) => [(domain, codomain)] -> BR.BinaryRelation domain codomain -> BR.BinaryRelation domain codomain
addRelations relations br = List.foldl' (\br' (domain, codomain) -> BR.addRelation domain codomain br') br relations

-- | Remove a list of relations from the provided BinaryRelation.
removeRelations :: (Ord domain, Ord codomain) => [(domain, codomain)] -> BR.BinaryRelation domain codomain -> BR.BinaryRelation domain codomain
removeRelations relations br = List.foldl' (\br' (domain, codomain) -> BR.removeRelation domain codomain br') br relations

-- * QUICKCHECK
-------------------------------------------------------------------------------

-- | Add all the elements to the domain and check if they were added with getDomain.
prop_addToDomain :: [Int] -> Bool
prop_addToDomain elements = BR.getDomain createdBR == insertedElementsSet where
	createdBR = addElementsToDomain elements (BR.empty :: BR.BinaryRelation Int Int)
	insertedElementsSet = Set.fromList elements

-- | Add all the elements to the codomain and check if they were added with getCodomain.
prop_addToCodomain :: [Int] -> Bool
prop_addToCodomain elements = BR.getCodomain createdBR == insertedElementsSet where
	createdBR = addElementsToCodomain elements (BR.empty :: BR.BinaryRelation Int Int)
	insertedElementsSet = Set.fromList elements

-- | Add all the elements to the domain and check the size of the added elements with getDomainCount.
prop_addToDomainCount :: [Int] -> Bool
prop_addToDomainCount elements = BR.getDomainCount createdBR == Set.size insertedElementsSet where
	createdBR = addElementsToDomain elements (BR.empty :: BR.BinaryRelation Int Int)
	insertedElementsSet = Set.fromList elements

-- | Add all the elements to the codomain and check the size of the added elements with getCodomainCount.
prop_addToCodomainCount :: [Int] -> Bool
prop_addToCodomainCount elements = BR.getCodomainCount createdBR == Set.size insertedElementsSet where
	createdBR = addElementsToCodomain elements (BR.empty :: BR.BinaryRelation Int Int)
	insertedElementsSet = Set.fromList elements

-- | Add all the elements to the domain and check if the codomain is empty.
prop_addToDomainCheckCodomain :: [Int] -> Bool
prop_addToDomainCheckCodomain elements = BR.getCodomain createdBR == Set.empty where
	createdBR = addElementsToDomain elements (BR.empty :: BR.BinaryRelation Int Int)

-- | Add all the elements to the codomain and check if the domain is empty.
prop_addToCodomainCheckDomain :: [Int] -> Bool
prop_addToCodomainCheckDomain elements = BR.getDomain createdBR == Set.empty where
	createdBR = addElementsToCodomain elements (BR.empty :: BR.BinaryRelation Int Int)

-- | Add all the elements to the domain and check if the codomain count is 0.
prop_addToDomainCheckCodomainCount :: [Int] -> Bool
prop_addToDomainCheckCodomainCount elements = BR.getCodomainCount createdBR == 0 where
	createdBR = addElementsToDomain elements (BR.empty :: BR.BinaryRelation Int Int)

-- | Add all the elements to the codomain and check if the domain count is 0.
prop_addToCodomainCheckDomainCount :: [Int] -> Bool
prop_addToCodomainCheckDomainCount elements = BR.getDomainCount createdBR == 0 where
	createdBR = addElementsToCodomain elements (BR.empty :: BR.BinaryRelation Int Int)
