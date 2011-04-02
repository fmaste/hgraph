-- Author: Federico Mastellone (fmaste@gmail.com)

module Data.Graph.Labels (
	Labels(),
	empty,
	addLabel,
	addElement,
	removeLabel,
	removeElement,
	addElementLabel,
	removeElementLabel,
	getLabels,
	getElements,
	getLabelsCount,
	getElementsCount,
	getLabelElements,
	getElementLabels,
	getLabelElementsCount,
	getElementLabelsCount) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Data.Set as Set

-- DATA DEFINITION
-------------------------------------------------------------------------------

-- Graphs whose edges or vertices have names or labels are known as labeled, 
-- those without as unlabeled. 
-- Labels can be for the nodes or the arcs. Graphs with labeled vertices only 
-- are vertex-labeled, those with labeled edges only are edge-labeled.
-- Nodes are already indentifiable, but labels can be used for what you want.
-- The difference between a edge-labeled and an edge-unlabeled graph is that 
-- the latter has no specific set of edges; it is regarded as another way to 
-- look upon an isomorphism type of graphs. (Thus, this usage distinguishes 
-- between graphs with identifiable edge sets on the one hand, and isomorphism 
-- types or classes of graphs on the other.)
-- We defined Labels with two structures, one with the label -> element 
-- relationships and the other with the element -> label.
data Labels element label = Labels (LabelElements element label) (ElementLabels element label)
    deriving (Show, Read, Ord, Eq)

-- A label may appear on any element.
-- A labels contains a Set of elements.
type LabelElements element label = Map.Map label (Set.Set element)

-- An element may contain any labels.
-- An element contains a Set of labels.
type ElementLabels element label = Map.Map element (Set.Set label)

-- CONSTRUCTION
-------------------------------------------------------------------------------

-- The empty Labels.
empty :: (Ord element, Ord label) => Labels element label
empty = Labels Map.empty Map.empty

-- Adds a label without any elements relationships.
-- If this label already exists the original Labels is returned.
addLabel :: (Ord element, Ord label) => label -> Labels element label -> Labels element label
addLabel label (Labels labelElements elementLabels) = Labels labelElements' elementLabels where
	labelElements' = Map.insertWith (\new old -> old) label Set.empty labelElements

-- Adds an element without any labels relationships.
-- If this element already exists the original Labels is returned.
addElement :: (Ord element, Ord label) => element -> Labels element label -> Labels element label
addElement element (Labels labelElements elementLabels) = Labels labelElements elementLabels' where
	elementLabels' = Map.insertWith (\new old -> old) element Set.empty elementLabels

-- Removes a label and all its elements relationships.
-- If this label does not exists the original Labels is returned.
removeLabel :: (Ord element, Ord label) => label -> Labels element label -> Labels element label
removeLabel label adj@(Labels labelElements elementLabels) = Labels labelElements' elementLabels' where
	labelElements' = Map.delete label labelElements
	elementLabels' = foldl f elementLabels $ getLabelElements label adj where
		f elementLabels'' element = Map.adjust (Set.delete label) element elementLabels''

-- Removes an element and all its labels relationships.
-- If this element does not exists the original Labels is returned.
removeElement :: (Ord element, Ord label) => element -> Labels element label -> Labels element label
removeElement element adj@(Labels labelElements elementLabels) = Labels labelElements' elementLabels' where
	labelElements' = foldl f labelElements $ getElementLabels element adj where
		f labelElements'' label = Map.adjust (Set.delete element) label labelElements''
	elementLabels' = Map.delete element elementLabels

-- Adds a label to the element.
-- TODO: Insert the element or label if they don't exist.
-- If one or more labels already existed for this element it is appended.
-- If this label already exists for this element the original Labels is returned.
addElementLabel :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
addElementLabel element label (Labels labelElements elementLabels) =
	let 
		labelElements' = f labelElements label element
		elementLabels' = f elementLabels element label
		f parentMap k v = Map.insertWith' g k (Set.singleton v) parentMap where
			g new old = Set.insert v old
	in Labels labelElements' elementLabels'

-- Removes a label from the element.
-- TODO: Insert the element or label if they don't exist.
-- If one or more labels already existed for this element only one is removed.
-- If this element already exists for this label the original Labels is returned.
removeElementLabel :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
removeElementLabel element label (Labels labelElements elementLabels) = 
	let 
		labelElements' = f labelElements label element
		elementLabels' = f elementLabels element label
		f parentMap k v = Map.adjust g k parentMap where
			g aSet = Set.delete v aSet
	in Labels labelElements' elementLabels'

-- QUERY
-------------------------------------------------------------------------------

getLabels :: (Ord element, Ord label) => Labels element label -> [label]
getLabels (Labels labelElements _) = Map.keys labelElements

getElements :: (Ord element, Ord label) => Labels element label -> [element]
getElements (Labels _ elementLabels) = Map.keys elementLabels

getLabelsCount :: (Ord element, Ord label) => Labels element label -> Int
getLabelsCount (Labels labelElements _) = Map.size labelElements

getElementsCount :: (Ord element, Ord label) => Labels element label -> Int
getElementsCount (Labels _ elementLabels) = Map.size elementLabels

getLabelElements :: (Ord element, Ord label) => label -> Labels element label -> [element]
getLabelElements label (Labels labelElements _) = 
	Set.elems $ Map.findWithDefault Set.empty label labelElements

getElementLabels :: (Ord element, Ord label) => element -> Labels element label -> [label]
getElementLabels element (Labels _ elementLabels) = 
	Set.elems $ Map.findWithDefault Set.empty element elementLabels

getLabelElementsCount :: (Ord element, Ord label) => label -> Labels element label -> Int
getLabelElementsCount label (Labels labelElements _) = 
	Set.size $ Map.findWithDefault Set.empty label labelElements

getElementLabelsCount :: (Ord element, Ord label) => element -> Labels element label -> Int
getElementLabelsCount element (Labels _ elementLabels) = 
	Set.size $ Map.findWithDefault Set.empty element elementLabels
