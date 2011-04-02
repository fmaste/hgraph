-- Author: Federico Mastellone (fmaste@gmail.com)

module Data.Graph.Labels (
	Labels(),
	empty,
	addLabel,
	addOrReplaceLabel,
	-- TODO: removeLabel,
	-- TODO: removeArc,
	removeLabel,
	removeLabelsAll,
	getLabels,
	getUniqueLabels,
	getElements,
	getUniqueElements,
	getLabelsCount,
	getUniqueLabelsCount,
	getElementsCount,
	getUniqueElementsCount,
	getLabelElements,
	getLabelUniqueElements,
	getElementLabels,
	getElementUniqueLabels) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Map as Map

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

-- A label may appear on any element and can be repeated.
type LabelElements element label = Map.Map label (Map.Map element Int)

-- An element can be repeated with equal or different labels.
type ElementLabels element label = Map.Map element (Map.Map label Int)

-- CONSTRUCTION
-------------------------------------------------------------------------------

-- The empty Labels.
empty :: (Ord element, Ord label) => Labels element label
empty = Labels Map.empty Map.empty

-- Adds a label to the element.
-- If one or more labels already existed for this element it is appended.
addLabel :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
addLabel element label (Labels labelElements elementLabels) =
	let 
		labelElements' = f labelElements label element
		elementLabels' = f elementLabels element label
		f parentMap k v = Map.insertWith' g k (Map.singleton v 1) parentMap where
			g new old = Map.adjust (+ 1) v old
			-- f = flip $ Map.unionWith (+) -- InsertWith calls f (new, old), but union is more efficinet with (bigger, smaller)
	in Labels labelElements' elementLabels'

-- Adds a label to the element.
-- If one or more labels already existed for this element they are replaced.
addOrReplaceLabel :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
addOrReplaceLabel element label (Labels labelElements elementLabels) = 
	let 
		labelElements' = f labelElements label element
		elementLabels' = f elementLabels element label
		f parentMap k v = Map.insertWith' g k (Map.singleton v 1) parentMap where
			g new old = old -- Same as flip $ const
	in Labels labelElements' elementLabels'

-- Removes a label from the element.
-- If one or more labels already existed for this element only one is removed.
removeLabel :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
removeLabel element label (Labels labelElements elementLabels) = 
	let 
		labelElements' = f labelElements label element
		elementLabels' = f elementLabels element label
		f parentMap k v = Map.update g k parentMap where
			g childMap
				| Map.size childMap == 1 && Map.member v childMap && childMap Map.! v <= 1 = Nothing
				| otherwise = Just $ Map.update g' v childMap where
					g' vCount
						| vCount <= 1 = Nothing
						| otherwise = Just $ vCount - 1
	in Labels labelElements' elementLabels'

-- Removes all the labels from the element.
-- If one or more labels already existed for this element they are all removed.
removeLabelsAll :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
removeLabelsAll element label (Labels labelElements elementLabels) = 
	let 
		labelElements' = f labelElements label element
		elementLabels' = f elementLabels element label
		f parentMap k v = Map.update g k parentMap where
			g childMap
				| Map.size childMap == 1 && Map.member v childMap = Nothing
				| otherwise = Just $ Map.delete v childMap
	in Labels labelElements' elementLabels'

{--
removeArc :: (Ord element, Ord label) => element -> element -> Labels element label -> Labels element label
removeArc src dst (Labels labelElements elementLabels) = Labels labelElements' elementLabels' where
	labelElements' = Map.foldWithKey f labelElements elementLabels where
		f label arcMap labelElements'' = foldl (removeLabelsAll src dst label) labelElements'' (Map.keys arcMap)		
	elementLabels' = Map.delete (src, dst) elementLabels

removeLabel :: (Ord element, Ord label) => label -> Labels element label -> Labels element label
removeLabel label (Labels labelElements elementLabels) = Labels labelElements' elementLabels' where
	labelElements' = Map.delete label labelElements
	elementLabels' = Map.foldWithKey f elementLabels labelElements where
		f (src, dst) edgeMap elementLabels'' = foldl (removeLabelsAll src dst label) elementLabels'' (Map.keys edgeMap)
--}

-- QUERY
-------------------------------------------------------------------------------

getLabels :: (Ord element, Ord label) => Labels element label -> [label]
getLabels (Labels labelElements _) = Map.foldWithKey f [] labelElements where
	f label arcMap ans = ans ++ replicateLabels where
		replicateLabels = Map.fold (\count labels -> labels ++ (replicate count label)) [] arcMap

getUniqueLabels :: (Ord element, Ord label) => Labels element label -> [label]
getUniqueLabels (Labels labelElements _) = Map.keys labelElements

getElements :: (Ord element, Ord label) => Labels element label -> [element]
getElements (Labels _ elementLabels) = Map.foldWithKey f [] elementLabels where
	f arc labelMap ans = ans ++ replicateArcs where
		replicateArcs = Map.fold (\count arcs -> arcs ++ (replicate count arc)) [] labelMap

getUniqueElements :: (Ord element, Ord label) => Labels element label -> [element]
getUniqueElements (Labels _ elementLabels) = Map.keys elementLabels

 -- TODO
getLabelsCount :: (Ord element, Ord label) => Labels element label -> Int
getLabelsCount (Labels labelElements _) = Map.size labelElements

getUniqueLabelsCount :: (Ord element, Ord label) => Labels element label -> Int
getUniqueLabelsCount (Labels labelElements _) = Map.size labelElements

-- TODO
getElementsCount :: (Ord element, Ord label) => Labels element label -> Int
getElementsCount (Labels _ elementLabels) = Map.size elementLabels

getUniqueElementsCount :: (Ord element, Ord label) => Labels element label -> Int
getUniqueElementsCount (Labels _ elementLabels) = Map.size elementLabels

getLabelElements :: (Ord element, Ord label) => label -> Labels element label -> [element]
getLabelElements label (Labels labelElements _) = 
	Map.foldWithKey f [] $ Map.findWithDefault Map.empty label labelElements where
		f arc count ans = ans ++ (replicate count arc)

getLabelUniqueElements :: (Ord element, Ord label) => label -> Labels element label -> [element]
getLabelUniqueElements label (Labels labelElements _) = 
	Map.keys $ Map.findWithDefault Map.empty label labelElements

getElementLabels :: (Ord element, Ord label) => element -> Labels element label -> [label]
getElementLabels element (Labels _ elementLabels) = 
	Map.foldWithKey f [] $ Map.findWithDefault Map.empty element elementLabels where
		f label count ans = ans ++ (replicate count label)

getElementUniqueLabels :: (Ord element, Ord label) => element -> Labels element label -> [label]
getElementUniqueLabels element (Labels _ elementLabels) = 
	Map.keys $ Map.findWithDefault Map.empty element elementLabels

{-- TODO
getArcLabelCount :: (Ord element, Ord label) => element -> element -> label -> Labels element label -> [(element, element)]
getArcLabelCount src dst label (Labels labelElements _) = 
	Map.findWithDefault $ Map.findWithDefault Map.empty label labelElements where
		f arc count ans = ans ++ (replicate count arc)
-}
