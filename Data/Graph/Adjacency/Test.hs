-- Author: Federico Mastellone (fmaste@gmail.com)

module Main where

import qualified Test.QuickCheck as QC
import Data.Graph.Adjacency.Quickcheck as AdjQC
import Text.Printf

main  = mapM_ (\(s,a) -> printf "%-30s: " s >> a) tests

tests  = [
	("Add node", QC.quickCheck AdjQC.prop_addNode),
	("Contains node", QC.quickCheck AdjQC.prop_containsNode),
	("Remove node", QC.quickCheck AdjQC.prop_removeNode),
	("Not contains node", QC.quickCheck AdjQC.prop_notContainsNode),
	("Remove node (empty)", QC.quickCheck AdjQC.prop_removeNodeFromEmpty),
	("Add node and adjacency", QC.quickCheck AdjQC.prop_addNodesAndAdjacency),
	("Add adjacency", QC.quickCheck AdjQC.prop_addAdjacency),
	("Contains adjacency", QC.quickCheck AdjQC.prop_containsAdjacency),
	("Contains node succ", QC.quickCheck AdjQC.prop_containsNodeSucc),
	("Contains node pred", QC.quickCheck AdjQC.prop_containsNodePred),
	("Remove adjacency", QC.quickCheck AdjQC.prop_removeAdjacency),
	("Not contains adjacency", QC.quickCheck AdjQC.prop_notContainsAdjacency),
	("Not contains node succ", QC.quickCheck AdjQC.prop_notContainsNodeSucc),
	("Not contains node pred", QC.quickCheck AdjQC.prop_notContainsNodePred),
	("Remove full adjacency", QC.quickCheck AdjQC.prop_removeFullAdjacency),
	("Not contains full adjacency", QC.quickCheck AdjQC.prop_notContainsFullAdjacency),
	("Remove node succs", QC.quickCheck AdjQC.prop_removeNodeSuccs),
	("Remove node preds", QC.quickCheck AdjQC.prop_removeNodePreds)]
