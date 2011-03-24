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

-- The labels (sometimes weight) of a graph is the data associated with an arc (the connection between two edges).
-- We defined Labels with two structures, one with the label -> arc relationships and the other with the arc -> label.
data Labels node edge = Labels (LabelArcs node edge) (ArcLabels node edge)
    deriving (Show, Read, Ord, Eq)

-- A label may appear on any arc and can be repeated.
type LabelArcs node edge = Map.Map edge (Map.Map (node, node) Int)

-- An arc can be repeated with equal or different labels.
type ArcLabels node edge = Map.Map (node, node) (Map.Map edge Int)

-- CONSTRUCTION
-------------------------------------------------------------------------------

-- The empty Labels.
empty :: (Ord node, Ord edge) => Labels node edge
empty = Labels Map.empty Map.empty

addArcLabel :: (Ord node, Ord edge) => node -> node -> edge -> Labels node edge -> Labels node edge
addArcLabel src dst edge (Labels labelArcs arcLabels) = Labels labelArcs' arcLabels' where
	labelArcs' = Map.insertWith' f edge       (Map.singleton (src, dst) 1) labelArcs where
		f new old = Map.adjust (+ 1) (src, dst) old
		-- f = flip $ Map.unionWith (+) -- InsertWith calls f (new, old), but union is more efficinet with (bigger, smaller)
	arcLabels' = Map.insertWith' g (src, dst) (Map.singleton edge       1) arcLabels where
		g new old = Map.adjust (+ 1) edge       old
		-- g = flip $ Map.unionWith (+) -- InsertWith calls f (new, old), but union is more efficinet with (bigger, smaller)
		
addOrReplaceArcLabel :: (Ord node, Ord edge) => node -> node -> edge -> Labels node edge -> Labels node edge
addOrReplaceArcLabel src dst edge (Labels labelArcs arcLabels) = Labels labelArcs' arcLabels' where
	labelArcs' = Map.insertWith' f edge       (Map.singleton (src, dst) 1) labelArcs where
		f new old = old -- Same as flip $ const
	arcLabels' = Map.insertWith' g (src, dst) (Map.singleton edge       1) arcLabels where
		g new old = old -- Same as flip $ const

removeArcLabel :: (Ord node, Ord edge) => node -> node -> edge -> Labels node edge -> Labels node edge
removeArcLabel src dst edge (Labels labelArcs arcLabels) = Labels labelArcs' arcLabels' where
	labelArcs' = Map.update f edge 		labelArcs where
		f arcsMap
			| Map.size arcsMap == 1 && Map.member (src, dst) arcsMap && arcsMap Map.! (src, dst) <= 1 = Nothing
			| otherwise = Just $ Map.update f' (src, dst) arcsMap where
				f' arcsCount
					| arcsCount <= 1 = Nothing
					| otherwise = Just $ arcsCount - 1
	arcLabels' = Map.update g (src, dst)    arcLabels where
		g labelsMap
			| Map.size labelsMap == 1 && Map.member edge labelsMap && labelsMap Map.! edge <= 1 = Nothing
			| otherwise = Just $ Map.update g' edge labelsMap where
				g' labelsCount
					| labelsCount <= 1 = Nothing
					| otherwise = Just $ labelsCount - 1

removeArcLabelsAll :: (Ord node, Ord edge) => node -> node -> edge -> Labels node edge -> Labels node edge
removeArcLabelsAll src dst edge (Labels labelArcs arcLabels) = Labels labelArcs' arcLabels' where
	labelArcs' = Map.update f edge 		labelArcs where
		f arcsMap
			| Map.size arcsMap == 1 && Map.member (src, dst) arcsMap = Nothing
			| otherwise = Just $ Map.delete (src, dst) arcsMap where
	arcLabels' = Map.update g (src, dst)    arcLabels where
		g labelsMap
			| Map.size labelsMap == 1 && Map.member edge labelsMap = Nothing
			| otherwise = Just $ Map.delete edge labelsMap where

-- QUERY
-------------------------------------------------------------------------------

getLabels :: (Ord node, Ord edge) => Labels node edge -> [edge]
getLabels (Labels labelArcs _) = Map.foldWithKey f [] labelArcs where
	f label arcMap ans = ans ++ replicateLabels where
		replicateLabels = Map.fold (\count labels -> labels ++ (replicate count label)) [] arcMap

getUniqueLabels :: (Ord node, Ord edge) => Labels node edge -> [edge]
getUniqueLabels (Labels labelArcs _) = Map.keys labelArcs

getArcs :: (Ord node, Ord edge) => Labels node edge -> [(node, node)]
getArcs (Labels _ arcLabels) = Map.foldWithKey f [] arcLabels where
	f arc labelMap ans = ans ++ replicateArcs where
		replicateArcs = Map.fold (\count arcs -> arcs ++ (replicate count arc)) [] labelMap

getUniqueArcs :: (Ord node, Ord edge) => Labels node edge -> [(node, node)]
getUniqueArcs (Labels _ arcLabels) = Map.keys arcLabels

 -- TODO
getLabelsCount :: (Ord node, Ord edge) => Labels node edge -> Int
getLabelsCount (Labels labelArcs _) = Map.size labelArcs

getUniqueLabelsCount :: (Ord node, Ord edge) => Labels node edge -> Int
getUniqueLabelsCount (Labels labelArcs _) = Map.size labelArcs

-- TODO
getArcsCount :: (Ord node, Ord edge) => Labels node edge -> Int
getArcsCount (Labels _ arcLabels) = Map.size arcLabels

getUniqueArcsCount :: (Ord node, Ord edge) => Labels node edge -> Int
getUniqueArcsCount (Labels _ arcLabels) = Map.size arcLabels

getLabelArcs :: (Ord node, Ord edge) => edge -> Labels node edge -> [(node, node)]
getLabelArcs edge (Labels labelArcs _) = 
	Map.foldWithKey f [] $ Map.findWithDefault Map.empty edge labelArcs where
		f arc count ans = ans ++ (replicate count arc)

getLabelUniqueArcs :: (Ord node, Ord edge) => edge -> Labels node edge -> [(node, node)]
getLabelUniqueArcs edge (Labels labelArcs _) = 
	Map.keys $ Map.findWithDefault Map.empty edge labelArcs

getArcLabels :: (Ord node, Ord edge) => node -> node -> Labels node edge -> [edge]
getArcLabels src dst (Labels _ arcLabels) = 
	Map.foldWithKey f [] $ Map.findWithDefault Map.empty (src, dst) arcLabels where
		f edge count ans = ans ++ (replicate count edge)

getArcUniqueLabels :: (Ord node, Ord edge) => node -> node -> Labels node edge -> [edge]
getArcUniqueLabels src dst (Labels _ arcLabels) = 
	Map.keys $ Map.findWithDefault Map.empty (src, dst) arcLabels

{-- TODO
getArcLabelCount :: (Ord node, Ord edge) => node -> node -> edge -> Labels node edge -> [(node, node)]
getArcLabelCount src dst edge (Labels labelArcs _) = 
	Map.findWithDefault $ Map.findWithDefault Map.empty edge labelArcs where
		f arc count ans = ans ++ (replicate count arc)
-}
