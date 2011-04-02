-- Author: Federico Mastellone (fmaste@gmail.com)

module Data.Graph.Labels (
	Labels(),
	empty,
	addArcLabel,
	addOrReplaceArcLabel,
	-- TODO: removeLabel,
	-- TODO: removeArc,
	removeArcLabel,
	removeArcLabelsAll,
	getLabels,
	getUniqueLabels,
	getArcs,
	getUniqueArcs,
	getLabelsCount,
	getUniqueLabelsCount,
	getArcsCount,
	getUniqueArcsCount,
	getLabelArcs,
	getLabelUniqueArcs,
	getArcLabels,
	getArcUniqueLabels) where

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

-- Adds a label to the arc that goes from src to dst.
-- If one or more labels already existed for this arc and edge it is appended.
addArcLabel :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
addArcLabel element label (Labels labelElements arcLabels) = Labels labelElements' arcLabels' where
	labelElements' = Map.insertWith' f label   (Map.singleton element 1) labelElements where
		f new old = Map.adjust (+ 1) element old
		-- f = flip $ Map.unionWith (+) -- InsertWith calls f (new, old), but union is more efficinet with (bigger, smaller)
	arcLabels' = Map.insertWith' g element (Map.singleton label   1) arcLabels where
		g new old = Map.adjust (+ 1) label   old
		-- g = flip $ Map.unionWith (+) -- InsertWith calls f (new, old), but union is more efficinet with (bigger, smaller)

-- Adds a label to the arc that goes from src to dst.
-- If one or more labels already existed for this arc and edge they are replaced.
addOrReplaceArcLabel :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
addOrReplaceArcLabel element label (Labels labelElements arcLabels) = Labels labelElements' arcLabels' where
	labelElements' = Map.insertWith' f label   (Map.singleton element 1) labelElements where
		f new old = old -- Same as flip $ const
	arcLabels' = Map.insertWith' g element (Map.singleton label   1) arcLabels where
		g new old = old -- Same as flip $ const

-- Removes a label from the arc that goes from src to dst.
-- If one or more labels already existed for this arc and edge only one is removed.
removeArcLabel :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
removeArcLabel element label (Labels labelElements arcLabels) = Labels labelElements' arcLabels' where
	labelElements' = Map.update f label 		labelElements where
		f arcsMap
			| Map.size arcsMap == 1 && Map.member element arcsMap && arcsMap Map.! element <= 1 = Nothing
			| otherwise = Just $ Map.update f' element arcsMap where
				f' arcsCount
					| arcsCount <= 1 = Nothing
					| otherwise = Just $ arcsCount - 1
	arcLabels' = Map.update g element arcLabels where
		g labelsMap
			| Map.size labelsMap == 1 && Map.member label labelsMap && labelsMap Map.! label <= 1 = Nothing
			| otherwise = Just $ Map.update g' label labelsMap where
				g' labelsCount
					| labelsCount <= 1 = Nothing
					| otherwise = Just $ labelsCount - 1

-- Removes all the labels from the arc that goes from src to dst that contain edge.
-- If one or more labels already existed for this arc and edge they are all removed.
removeArcLabelsAll :: (Ord element, Ord label) => element -> label -> Labels element label -> Labels element label
removeArcLabelsAll element label (Labels labelElements arcLabels) = Labels labelElements' arcLabels' where
	labelElements' = Map.update f label   labelElements where
		f arcsMap
			| Map.size arcsMap == 1 && Map.member element arcsMap = Nothing
			| otherwise = Just $ Map.delete element arcsMap where
	arcLabels' = Map.update g element arcLabels where
		g labelsMap
			| Map.size labelsMap == 1 && Map.member label labelsMap = Nothing
			| otherwise = Just $ Map.delete label labelsMap where

{--
removeArc :: (Ord element, Ord label) => element -> element -> Labels element label -> Labels element label
removeArc src dst (Labels labelElements arcLabels) = Labels labelElements' arcLabels' where
	labelElements' = Map.foldWithKey f labelElements arcLabels where
		f label arcMap labelElements'' = foldl (removeArcLabelsAll src dst label) labelElements'' (Map.keys arcMap)		
	arcLabels' = Map.delete (src, dst) arcLabels

removeLabel :: (Ord element, Ord label) => label -> Labels element label -> Labels element label
removeLabel label (Labels labelElements arcLabels) = Labels labelElements' arcLabels' where
	labelElements' = Map.delete label labelElements
	arcLabels' = Map.foldWithKey f arcLabels labelElements where
		f (src, dst) edgeMap arcLabels'' = foldl (removeArcLabelsAll src dst label) arcLabels'' (Map.keys edgeMap)
--}

-- QUERY
-------------------------------------------------------------------------------

getLabels :: (Ord element, Ord label) => Labels element label -> [label]
getLabels (Labels labelElements _) = Map.foldWithKey f [] labelElements where
	f label arcMap ans = ans ++ replicateLabels where
		replicateLabels = Map.fold (\count labels -> labels ++ (replicate count label)) [] arcMap

getUniqueLabels :: (Ord element, Ord label) => Labels element label -> [label]
getUniqueLabels (Labels labelElements _) = Map.keys labelElements

getArcs :: (Ord element, Ord label) => Labels element label -> [element]
getArcs (Labels _ arcLabels) = Map.foldWithKey f [] arcLabels where
	f arc labelMap ans = ans ++ replicateArcs where
		replicateArcs = Map.fold (\count arcs -> arcs ++ (replicate count arc)) [] labelMap

getUniqueArcs :: (Ord element, Ord label) => Labels element label -> [element]
getUniqueArcs (Labels _ arcLabels) = Map.keys arcLabels

 -- TODO
getLabelsCount :: (Ord element, Ord label) => Labels element label -> Int
getLabelsCount (Labels labelElements _) = Map.size labelElements

getUniqueLabelsCount :: (Ord element, Ord label) => Labels element label -> Int
getUniqueLabelsCount (Labels labelElements _) = Map.size labelElements

-- TODO
getArcsCount :: (Ord element, Ord label) => Labels element label -> Int
getArcsCount (Labels _ arcLabels) = Map.size arcLabels

getUniqueArcsCount :: (Ord element, Ord label) => Labels element label -> Int
getUniqueArcsCount (Labels _ arcLabels) = Map.size arcLabels

getLabelArcs :: (Ord element, Ord label) => label -> Labels element label -> [element]
getLabelArcs label (Labels labelElements _) = 
	Map.foldWithKey f [] $ Map.findWithDefault Map.empty label labelElements where
		f arc count ans = ans ++ (replicate count arc)

getLabelUniqueArcs :: (Ord element, Ord label) => label -> Labels element label -> [element]
getLabelUniqueArcs label (Labels labelElements _) = 
	Map.keys $ Map.findWithDefault Map.empty label labelElements

getArcLabels :: (Ord element, Ord label) => element -> Labels element label -> [label]
getArcLabels element (Labels _ arcLabels) = 
	Map.foldWithKey f [] $ Map.findWithDefault Map.empty element arcLabels where
		f label count ans = ans ++ (replicate count label)

getArcUniqueLabels :: (Ord element, Ord label) => element -> Labels element label -> [label]
getArcUniqueLabels element (Labels _ arcLabels) = 
	Map.keys $ Map.findWithDefault Map.empty element arcLabels

{-- TODO
getArcLabelCount :: (Ord element, Ord label) => element -> element -> label -> Labels element label -> [(element, element)]
getArcLabelCount src dst label (Labels labelElements _) = 
	Map.findWithDefault $ Map.findWithDefault Map.empty label labelElements where
		f arc count ans = ans ++ (replicate count arc)
-}
