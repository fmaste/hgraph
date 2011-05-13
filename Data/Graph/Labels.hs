-- Author: Federico Mastellone (fmaste@gmail.com)

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
-- In short, this module tells if things of different type are related or not.
module Data.Graph.Labels (
	Labels(),
	empty,
	addLabel,
	addElement,
	removeLabel,
	removeElement,
	addLabelToElement,
	removeElementLabel,
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
import qualified Data.MultiMap as MM

-- DATA DEFINITION
-------------------------------------------------------------------------------

-- We defined Labels with two structures, one with the label -> element 
-- relationships and the other with the element -> label.
data Labels element label = Labels (LabelElements element label) (ElementLabels element label)
    deriving (Show, Read, Ord, Eq)

-- A label may appear on any element.
-- A labels contains a Set of elements.
type LabelElements element label = MM.MultiMap label element

-- An element may contain any labels.
-- An element contains a Set of labels.
type ElementLabels element label = MM.MultiMap element label

-- CONSTRUCTION
-------------------------------------------------------------------------------

-- The empty Labels.
empty :: (Ord element, Ord label) => Labels element label
empty = Labels MM.empty MM.empty

-- Adds a label without any elements relationships.
-- If this label already exists the original Labels is returned.
addLabel :: (Ord element, Ord label) => label -> Labels element label -> Labels element label
addLabel label (Labels labelElements elementLabels) = Labels labelElements' elementLabels where
	labelElements' = MM.addKey label labelElements

-- Adds an element without any labels relationships.
-- If this element already exists the original Labels is returned.
addElement :: (Ord element, Ord label) => element -> Labels element label -> Labels element label
addElement element (Labels labelElements elementLabels) = Labels labelElements elementLabels' where
	elementLabels' = MM.addKey element elementLabels

-- Removes a label and all its elements relationships.
-- If this label does not exists the original Labels is returned.
removeLabel :: (Ord element, Ord label) => label -> Labels element label -> Labels element label
removeLabel label adj@(Labels labelElements elementLabels) = Labels labelElements' elementLabels' where
	labelElements' = MM.removeKey label labelElements
	elementLabels' = foldl f elementLabels $ getLabelElements label adj where
		f elementLabels'' element = MM.removeValue element label elementLabels''

-- Removes an element and all its labels relationships.
-- If this element does not exists the original Labels is returned.
removeElement :: (Ord element, Ord label) => element -> Labels element label -> Labels element label
removeElement element adj@(Labels labelElements elementLabels) = Labels labelElements' elementLabels' where
	labelElements' = foldl f labelElements $ getElementLabels element adj where
		f labelElements'' label = MM.removeValue label element labelElements''
	elementLabels' = MM.removeKey element elementLabels

-- Adds a label to the element.
-- If the element or label don't exist they are added.
-- If one or more labels already existed for this element it is appended.
-- If this label already exists for this element the original Labels is returned.
addLabelToElement :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
addLabelToElement element label (Labels labelElements elementLabels) =
	let 
		labelElements' = MM.addValue label element labelElements
		elementLabels' = MM.addValue element label elementLabels
	in Labels labelElements' elementLabels'

-- Removes a label from the element.
-- If the element or label don't exist the original Labels is returned.
-- If one or more labels already existed for this element only one is removed.
-- If this element already exists for this label the original Labels is returned.
removeElementLabel :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
removeElementLabel element label (Labels labelElements elementLabels) = 
	let 
		labelElements' = MM.removeValue label element labelElements
		elementLabels' = MM.removeValue element label elementLabels
	in Labels labelElements' elementLabels'

-- QUERY
-------------------------------------------------------------------------------

getLabels :: (Ord element, Ord label) => Labels element label -> [label]
getLabels (Labels labelElements _) = MM.getKeys labelElements

getElements :: (Ord element, Ord label) => Labels element label -> [element]
getElements (Labels _ elementLabels) = MM.getKeys elementLabels

getLabelsCount :: (Ord element, Ord label) => Labels element label -> Int
getLabelsCount (Labels labelElements _) = MM.getKeyCount labelElements

getElementsCount :: (Ord element, Ord label) => Labels element label -> Int
getElementsCount (Labels _ elementLabels) = MM.getKeyCount elementLabels

getLabelElements :: (Ord element, Ord label) => label -> Labels element label -> [element]
getLabelElements label (Labels labelElements _) = MM.getValues label labelElements

getElementLabels :: (Ord element, Ord label) => element -> Labels element label -> [label]
getElementLabels element (Labels _ elementLabels) = MM.getValues element elementLabels

getLabelElementsSet :: (Ord element, Ord label) => label -> Labels element label -> Set.Set element
getLabelElementsSet label (Labels labelElements _) = MM.getValuesSet label labelElements

getElementLabelsSet :: (Ord element, Ord label) => element -> Labels element label -> Set.Set label
getElementLabelsSet element (Labels _ elementLabels) = MM.getValuesSet element elementLabels

getLabelElementsCount :: (Ord element, Ord label) => label -> Labels element label -> Int
getLabelElementsCount label (Labels labelElements _) = MM.getValueCount label labelElements

getElementLabelsCount :: (Ord element, Ord label) => element -> Labels element label -> Int
getElementLabelsCount element (Labels _ elementLabels) = MM.getValueCount element elementLabels

containsLabel :: (Ord element, Ord label) => label -> Labels element label -> Bool
containsLabel label (Labels labelElements _) = MM.containsKey label labelElements

containsElement :: (Ord element, Ord label) => element -> Labels element label -> Bool
containsElement element (Labels _ elementLabels) = MM.containsKey element elementLabels

labelContainsElement :: (Ord element, Ord label) => label -> element -> Labels element label -> Bool
labelContainsElement label element (Labels labelElements _) = MM.containsValue label element labelElements

elementContainsLabel :: (Ord element, Ord label) => element -> label -> Labels element label -> Bool
elementContainsLabel element label (Labels _ elementLabels) = MM.containsValue element label elementLabels
