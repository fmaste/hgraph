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
	getElementLabels) where

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

addLabel :: (Ord element, Ord label) => label -> Labels element label -> Labels element label
addLabel label (Labels labelElements elementLabels) = Labels labelElements' elementLabels where
	labelElements' = Map.insert label Set.empty labelElements

addElement :: (Ord element, Ord label) => element -> Labels element label -> Labels element label
addElement element (Labels labelElements elementLabels) = Labels labelElements elementLabels' where
	elementLabels' = Map.insert element Set.empty elementLabels

removeLabel :: (Ord element, Ord label) => label -> Labels element label -> Labels element label
removeLabel label adj@(Labels labelElements elementLabels) = Labels labelElements' elementLabels' where
	labelElements' = Map.delete label labelElements
	elementLabels' = foldl f elementLabels $ getLabelElements label adj where
		f elementLabels'' element = Map.adjust (Set.delete label) element elementLabels''

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

{--
removeArc :: (Ord element, Ord label) => element -> element -> Labels element label -> Labels element label
removeArc src dst (Labels labelElements elementLabels) = Labels labelElements' elementLabels' where
	labelElements' = Map.foldWithKey f labelElements elementLabels where
		f label arcMap labelElements'' = foldl (removeElementLabelsAll src dst label) labelElements'' (Map.keys arcMap)		
	elementLabels' = Map.delete (src, dst) elementLabels
--}

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

{-- TODO
getArcLabelCount :: (Ord element, Ord label) => element -> element -> label -> Labels element label -> [(element, element)]
getArcLabelCount src dst label (Labels labelElements _) = 
	Map.findWithDefault $ Map.findWithDefault Map.empty label labelElements where
		f arc count ans = ans ++ (replicate count arc)
-}
