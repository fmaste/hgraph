-- Author: Federico Mastellone (fmaste@gmail.com)

-- Generic module to manage elements and its labels.
-- Let you manage relationships between two kinds of things.
module Data.Graph.Labels (
	Labels(),
	empty,
	addLabel,
	addElement,
	removeLabel,
	removeElement,
	addLabelToElement,
	removeLabelFromElement,
	getLabels,
	getElements,
	getLabelsCount,
	getElementsCount,
	getLabelElements,
	getElementLabels,
	getLabelElementsSet,
	getElementLabelsSet,
	getLabelElementsCount,
	getElementLabelsCount,
	containsLabel,
	containsElement,
	labelContainsElement,
	elementContainsLabel) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Set as Set
import qualified Data.Collection.Relation.Binary as DCRB
import qualified Data.Collection.Relation.Binary.Domain as DCRBD
import qualified Data.Collection.Relation.Binary.Codomain as DCRBC
import qualified Data.Collection.Relation.Binary.Double as BR

-- DATA DEFINITION
-------------------------------------------------------------------------------

-- We defined Labels with two structures, one with the label -> element 
-- relationships and the other with the element -> label.
newtype Labels element label = Labels (BR.BinaryRelation element label)
    deriving (Show, Read, Ord, Eq)

-- CONSTRUCTION
-------------------------------------------------------------------------------

-- The empty Labels.
empty :: (Ord element, Ord label) => Labels element label
empty = Labels BR.empty

-- Adds a label without any elements relationships.
-- If this label already exists the original Labels is returned.
addLabel :: (Ord element, Ord label) => label -> Labels element label -> Labels element label
addLabel label (Labels br) = Labels br' where
	br' = DCRB.addCodomainElement label br

-- Adds an element without any labels relationships.
-- If this element already exists the original Labels is returned.
addElement :: (Ord element, Ord label) => element -> Labels element label -> Labels element label
addElement element (Labels br) = Labels br' where
	br' = DCRB.addDomainElement element br

-- Removes a label and all its elements relationships.
-- If this label does not exists the original Labels is returned.
removeLabel :: (Ord element, Ord label) => label -> Labels element label -> Labels element label
removeLabel label (Labels br) = Labels br' where
	br' = DCRB.removeCodomainElement label br

-- Removes an element and all its labels relationships.
-- If this element does not exists the original Labels is returned.
removeElement :: (Ord element, Ord label) => element -> Labels element label -> Labels element label
removeElement element (Labels br) = Labels br' where
	br' = DCRB.removeDomainElement element br

-- Adds a label to the element.
-- If the element or label don't exist they are added.
-- If one or more labels already existed for this element it is appended.
-- If this label already exists for this element the original Labels is returned.
addLabelToElement :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
addLabelToElement element label (Labels br) = Labels br' where
	br' = DCRB.addRelation element label br

-- Removes a label from the element.
-- If the element or label don't exist the original Labels is returned.
-- If one or more labels already existed for this element only one is removed.
-- If this element already exists for this label the original Labels is returned.
removeLabelFromElement :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
removeLabelFromElement element label (Labels br) = Labels br' where
	br' = DCRB.removeRelation element label br

-- QUERY
-------------------------------------------------------------------------------

getLabels :: (Ord element, Ord label) => Labels element label -> [label]
getLabels (Labels br) = DCRBC.getCodomainList br

getElements :: (Ord element, Ord label) => Labels element label -> [element]
getElements (Labels br) = DCRBD.getDomainList br

getLabelsCount :: (Ord element, Ord label) => Labels element label -> Integer
getLabelsCount (Labels br) = DCRBC.getCodomainCount br

getElementsCount :: (Ord element, Ord label) => Labels element label -> Integer
getElementsCount (Labels br) = DCRBD.getDomainCount br

getLabelElements :: (Ord element, Ord label) => label -> Labels element label -> [element]
getLabelElements label (Labels br) = DCRBC.getRelatedFromList label br

getElementLabels :: (Ord element, Ord label) => element -> Labels element label -> [label]
getElementLabels element (Labels br) = DCRBD.getRelatedToList element br

getLabelElementsSet :: (Ord element, Ord label) => label -> Labels element label -> Set.Set element
getLabelElementsSet label (Labels br) = DCRB.getRelatedFrom label br

getElementLabelsSet :: (Ord element, Ord label) => element -> Labels element label -> Set.Set label
getElementLabelsSet element (Labels br) = DCRB.getRelatedTo element br

getLabelElementsCount :: (Ord element, Ord label) => label -> Labels element label -> Integer
getLabelElementsCount label (Labels br) = DCRBC.getRelatedFromCount label br

getElementLabelsCount :: (Ord element, Ord label) => element -> Labels element label -> Integer
getElementLabelsCount element (Labels br) = DCRBD.getRelatedToCount element br

containsLabel :: (Ord element, Ord label) => label -> Labels element label -> Bool
containsLabel label (Labels br) = DCRBC.containsCodomainElement label br

containsElement :: (Ord element, Ord label) => element -> Labels element label -> Bool
containsElement element (Labels br) = DCRBD.containsDomainElement element br

labelContainsElement :: (Ord element, Ord label) => label -> element -> Labels element label -> Bool
labelContainsElement label element (Labels br) = DCRBC.isRelatedFrom label element br

elementContainsLabel :: (Ord element, Ord label) => element -> label -> Labels element label -> Bool
elementContainsLabel element label (Labels br) = DCRBD.isRelatedTo element label br

