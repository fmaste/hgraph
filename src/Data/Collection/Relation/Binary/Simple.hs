-- Author: Federico Mastellone (fmaste@gmail.com)

-- Generic module to manage binary relationships.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
module Data.Collection.Relation.Binary.Simple (
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

import qualified Data.List as DL
import qualified Data.Collection as DC
import qualified Data.Collection.Cardinality as DCC
import qualified Data.Collection.Import as DCI
import qualified Data.Collection.Export as DCE
import qualified Data.Collection.Map as DCM
import qualified Data.Collection.Map.Keys as DCMK
import qualified Data.Collection.Map.Foldable as DCMF
import qualified Data.Collection.Map.Multi as DCMM
import qualified Data.Collection.Map.Multi.Set as DCMMS
import qualified Data.Collection.Relation.Binary as DCRB
import qualified Data.Collection.Relation.Binary.Domain as DCRBD
import qualified Data.Collection.Relation.Binary.Codomain as DCRBC
import qualified Data.Collection.Relation.Binary.Theory as DCRBT
import qualified Data.Collection.Map.Multi.Set.Standard as MapSet
import qualified Data.Collection.Set.Standard as Set

-- DATA DEFINITION
-------------------------------------------------------------------------------

-- We defined BinaryRelation with two structures, one with the domain -> codomain 
-- relationships and the other is just a set of codomain elements. It stores 
-- the minimun information but the codomain functions are slower.
data BinaryRelation domain codomain = BinaryRelation (MapSet.MapSet domain codomain) (Set.Set codomain)
    deriving (Show, Read, Ord, Eq)

-- ATOMIC CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- The empty binary relation.
empty :: (Ord domain, Ord codomain) => BinaryRelation domain codomain
empty = BinaryRelation MapSet.empty Set.empty

-- Adds an element to the domain.
-- If this element already exists the original BinaryRelation is returned.
addDomainElement :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
addDomainElement element (BinaryRelation relatedTo codomain) = BinaryRelation relatedTo' codomain where
	relatedTo' = DCMM.addKey element relatedTo

-- Adds an element to the codomain.
-- If this element already exists the original BinaryRelation is returned.
addCodomainElement :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
addCodomainElement element (BinaryRelation relatedTo codomain) = BinaryRelation relatedTo codomain' where
	codomain' = Set.addElement element codomain

-- Removes an element from the domain.
-- If this element does not exists the original BinaryRelation is returned.
removeDomainElement :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
removeDomainElement element (BinaryRelation relatedTo codomain) = BinaryRelation relatedTo' codomain where
	relatedTo' = DCM.removeKey element relatedTo

-- Removes an element from the codomain.
-- If this element does not exists the original BinaryRelation is returned.
removeCodomainElement :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
removeCodomainElement element (BinaryRelation relatedTo codomain) = 
	let
		relatedTo' = DCMM.remove element relatedTo
		codomain' = Set.removeElement element codomain
	in BinaryRelation relatedTo' codomain'

-- Adds a relation from a domain element to a codomain one.
-- If the domain or codomain element is not present they are added.
-- If this relation already exists for this elements the original BinaryRelation is returned.
addRelation :: (Ord domain, Ord codomain) => domain -> codomain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
addRelation elementD elementC (BinaryRelation relatedTo codomain) =
	let 
		relatedTo' = DCMM.addToKey elementD elementC relatedTo
		-- TODO: Make a simpler version that does not checks if the values were already inserted.
		codomain' = Set.addElement elementC codomain
	in BinaryRelation relatedTo' codomain'

-- Removes a relation from a domain element to a codomain one.
-- If this relation is not present the original BinaryRelation is returned.
removeRelation :: (Ord domain, Ord codomain) => domain -> codomain -> BinaryRelation domain codomain -> BinaryRelation domain codomain
removeRelation elementD elementC (BinaryRelation relatedTo codomain) = BinaryRelation relatedTo' codomain where
	relatedTo' = DCMM.removeFromKey elementD elementC relatedTo

-- ATOMIC QUERY FUNCTIONS
-------------------------------------------------------------------------------

getDomain :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Set.Set domain
getDomain (BinaryRelation relatedTo _) = Set.fromList $ DCM.getKeys relatedTo

getCodomain :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Set.Set codomain
getCodomain (BinaryRelation _ codomain) = codomain

getRelatedTo :: (Ord domain, Ord codomain) => domain -> BinaryRelation domain codomain -> Set.Set codomain
getRelatedTo element (BinaryRelation relatedTo _) = DCM.getValueWithDefault Set.empty element relatedTo

getRelatedFrom :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> Set.Set domain
getRelatedFrom element (BinaryRelation relatedTo _) = DCMF.foldrWithKey f Set.empty relatedTo where
	f key set ans = if Set.containsElement element set then Set.addElement key ans else ans

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
getCodomainList (BinaryRelation _ codomain) = DCE.toList codomain

getCodomainCount :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Int
getCodomainCount (BinaryRelation _ codomain) = fromInteger $ DCC.getElementsCount codomain

containsCodomainElement :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> Bool
containsCodomainElement element (BinaryRelation _ codomain) = Set.containsElement element codomain

getRelatedFromList :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> [domain]
getRelatedFromList element br = DCE.toList $ getRelatedFrom element br

getRelatedFromCount :: (Ord domain, Ord codomain) => codomain -> BinaryRelation domain codomain -> Int
getRelatedFromCount element br = fromInteger $ DCC.getElementsCount $ getRelatedFrom element br

isRelatedFrom :: (Ord domain, Ord codomain) => codomain -> domain -> BinaryRelation domain codomain -> Bool
isRelatedFrom codomain domain br = containsRelation domain codomain br

-- RELATION THEORY
-------------------------------------------------------------------------------

isInjective :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> Bool
isInjective br = all (\codomain -> getRelatedFromCount codomain br <= 1) $ getCodomainList br

revert :: (Ord domain, Ord codomain) => BinaryRelation domain codomain -> BinaryRelation codomain domain
revert br@(BinaryRelation relatedTo codomain) = BinaryRelation relatedTo' codomain' where
	relatedTo' = DL.foldl' f MapSet.empty $ getCodomainList br where
		f mm elementC = DL.foldl' g mm $ getRelatedFromList elementC br where
			g mm elementD = DCMM.addToKey elementC elementD mm
	codomain' = getDomain br

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

instance (Ord domain, Ord codomain) => DCRBT.Theory (BinaryRelation domain codomain) where
	isInjective = isInjective

