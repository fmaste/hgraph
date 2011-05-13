-- Author: Federico Mastellone (fmaste@gmail.com)

module Data.Graph.Adjacency.Quickcheck (
	prop_addNode,
	prop_containsNode,
	prop_removeNode,
	prop_notContainsNode,
	prop_removeNodeFromEmpty,
	prop_addNodesAndAdjacency,
	prop_addAdjacency,
	prop_containsAdjacency,
	prop_containsNodeSucc,
	prop_containsNodePred,
	prop_removeAdjacency,
	prop_notContainsAdjacency,
	prop_notContainsNodeSucc,
	prop_notContainsNodePred,
	prop_removeFullAdjacency,
	prop_notContainsFullAdjacency,
	prop_removeNodeSuccs,
	prop_removeNodePreds) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Graph.Adjacency as Adj
import qualified Test.QuickCheck as QC
import qualified Data.List as List

-- * UTILS
-------------------------------------------------------------------------------

-- | Add a list of nodes to the provided Adjacency.
addNodeList :: Ord node => [node] -> Adj.Adjacency node -> Adj.Adjacency node
addNodeList nodes adj = foldl (\adj' node -> Adj.addNode node adj') adj nodes

-- | Remove a list of nodes from the provided Adjacency.
removeNodeList :: Ord node => [node] -> Adj.Adjacency node -> Adj.Adjacency node
removeNodeList nodes adj = foldl (\adj' node -> Adj.removeNode node adj') adj nodes

-- | Adds the nodes on the list of adjacencies to the provided Adjacency.
addNodesOnArcList :: Ord node => [(node, node)] -> Adj.Adjacency node -> Adj.Adjacency node
addNodesOnArcList arcs adj = foldl (\adj' (src, dst) -> Adj.addNode src $ Adj.addNode dst adj') adj arcs

-- | Adds the list of adjacencies to the provided Adjacency.
addArcList :: Ord node => [(node, node)] -> Adj.Adjacency node -> Adj.Adjacency node
addArcList arcs adj = foldl (\adj' arc -> uncurry Adj.addAdjacency arc adj') adj arcs

-- | Removes the list of adjacencies from the provided Adjacency.
removeArcList :: Ord node => [(node, node)] -> Adj.Adjacency node -> Adj.Adjacency node
removeArcList arcs adj = foldl (\adj' arc -> uncurry Adj.removeAdjacency arc adj') adj arcs

-- | Removes fully the list of adjacencies from the provided Adjacency.
removeFullArcList :: Ord node => [(node, node)] -> Adj.Adjacency node -> Adj.Adjacency node
removeFullArcList arcs adj = foldl (\adj' arc -> uncurry Adj.removeFullAdjacency arc adj') adj arcs

-- * QUICKCHECK
-------------------------------------------------------------------------------

-- | Add all the nodes and check if they were added with getNodes.
prop_addNode :: [Int] -> Bool
prop_addNode nodes = nonRepeatInsertedNodesList == nodesFromCreatedAdjacency where
	nonRepeatInsertedNodesList = List.sort $ List.nub nodes
	nodesFromCreatedAdjacency = List.sort $ Adj.getNodes $ addNodeList nodes Adj.empty

-- | Add all the nodes and check if they were added with containsNode.
prop_containsNode :: [Int] -> Bool
prop_containsNode nodes = all id [ Adj.containsNode x adj | x <- nodes] where
	adj = addNodeList nodes Adj.empty

-- | Remove all the nodes, after adding them, and check if they were removed with getNodes.
prop_removeNode :: [Int] -> Bool
prop_removeNode nodes = nodesFromCreatedAdjacency == [] where
	nodesFromCreatedAdjacency = List.sort $ Adj.getNodes $ removeNodeList nodes $ addNodeList nodes Adj.empty

-- | Remove all the nodes, after adding them, and check if they were removed with containsNode.
prop_notContainsNode :: [Int] -> Bool
prop_notContainsNode nodes = not $ any id [ Adj.containsNode x adj | x <- nodes] where
	adj = removeNodeList nodes $ addNodeList nodes Adj.empty

-- | Remove all the nodes from an empty Adjacency and check if it is still empty with getNodes.
prop_removeNodeFromEmpty :: [Int] -> Bool
prop_removeNodeFromEmpty nodes = nodesFromEmptyAdjacency == [] where
	nodesFromEmptyAdjacency = List.sort $ Adj.getNodes $ removeNodeList nodes Adj.empty

-- | Add all the adjacencies, after adding the nodes, and check if they were added with getAdjacencies.
prop_addNodesAndAdjacency :: [(Int, Int)] -> Bool
prop_addNodesAndAdjacency arcs = nonRepeatInsertedAdjacenciesList == adjacenciesFromCreatedAdjacency where
	nonRepeatInsertedAdjacenciesList = List.sort $ List.nub arcs
	adjacenciesFromCreatedAdjacency = List.sort $ Adj.getAdjacencies $ addArcList arcs $ addNodesOnArcList arcs Adj.empty

-- | Add all the adjacencies, without adding the nodes, and check if they were added with getAdjacencies.
prop_addAdjacency :: [(Int, Int)] -> Bool
prop_addAdjacency arcs = nonRepeatInsertedAdjacenciesList == adjacenciesFromCreatedAdjacency where
	nonRepeatInsertedAdjacenciesList = List.sort $ List.nub arcs
	adjacenciesFromCreatedAdjacency = List.sort $ Adj.getAdjacencies $ addArcList arcs Adj.empty

-- | Add all the adjacencies, without adding the nodes, and check if they were added with containsAdjacency.
prop_containsAdjacency :: [(Int, Int)] -> Bool
prop_containsAdjacency arcs = all id [ Adj.containsAdjacency src dst adj | (src, dst) <- arcs] where
	adj = addArcList arcs Adj.empty

-- | Add all the adjacencies, without adding the nodes, and check if they were added with containsNodeSucc.
prop_containsNodeSucc :: [(Int, Int)] -> Bool
prop_containsNodeSucc arcs = all id [ Adj.containsNodeSucc src dst adj | (src, dst) <- arcs] where
	adj = addArcList arcs Adj.empty

-- | Add all the adjacencies, without adding the nodes, and check if they were added with containsNodePred.
prop_containsNodePred :: [(Int, Int)] -> Bool
prop_containsNodePred arcs = all id [ Adj.containsNodePred dst src adj | (src, dst) <- arcs] where
	adj = addArcList arcs Adj.empty

-- | Remove all the adjacencies, after adding them, and check if they were removed with getAdjacencies.
prop_removeAdjacency :: [(Int, Int)] -> Bool
prop_removeAdjacency adjacencies = adjacenciesFromCreatedAdjacency == [] where
	adjacenciesFromCreatedAdjacency = Adj.getAdjacencies $ removeArcList adjacencies $ addArcList adjacencies Adj.empty

-- | Remove all the adjacencies, after adding them, and check if they were removed with containsAdjacency.
prop_notContainsAdjacency :: [(Int, Int)] -> Bool
prop_notContainsAdjacency arcs = not $ any id [ Adj.containsAdjacency src dst adj | (src, dst) <- arcs] where
	adj = removeArcList arcs $ addArcList arcs Adj.empty

-- | Remove all the adjacencies, after adding them, and check if they were removed with containsNodeSucc.
prop_notContainsNodeSucc :: [(Int, Int)] -> Bool
prop_notContainsNodeSucc arcs = not $ any id [ Adj.containsNodeSucc src dst adj | (src, dst) <- arcs] where
	adj = removeArcList arcs $ addArcList arcs Adj.empty

-- | Remove all the adjacencies, after adding them, and check if they were removed with containsNodePred.
prop_notContainsNodePred :: [(Int, Int)] -> Bool
prop_notContainsNodePred arcs = not $ any id [ Adj.containsNodePred dst src adj | (src, dst) <- arcs] where
	adj = removeArcList arcs $ addArcList arcs Adj.empty

-- | Remove all the adjacencies fully, after adding them, and check if they were fully removed with getAdjacencies.
prop_removeFullAdjacency :: [(Int, Int)] -> Bool
prop_removeFullAdjacency adjacencies = adjacenciesFromCreatedAdjacency == [] where
	adjacenciesFromCreatedAdjacency = Adj.getAdjacencies $ removeFullArcList (revertAdjs adjacencies) $ addArcList adjacencies Adj.empty where
		revertAdjs adjs = foldl (\ans (src, dst) -> ans ++ [(dst, src)]) [] adjs

-- | Remove all the reverse adjacencies fully, after adding them correctly, and check if they were removed with containsAdjacency.
prop_notContainsFullAdjacency :: [(Int, Int)] -> Bool
prop_notContainsFullAdjacency arcs = not $ any id [ (Adj.containsAdjacency src dst adj) && (Adj.containsAdjacency dst src adj) | (src, dst) <- arcs] where
	adj = removeFullArcList arcs $ addArcList arcs Adj.empty

-- | Remove all the node's succ adjacencies and check if they were removed with getAdjacencies.
prop_removeNodeSuccs :: [(Int, Int)] -> Int -> Bool
prop_removeNodeSuccs arcs node = (nodesSuccsFromCreatedAdjacency adj == []) && (not $ nodeIsInPreds adj) where
	adj = Adj.removeNodeSuccAdjacencies node $ addArcList arcs Adj.empty
	nodesSuccsFromCreatedAdjacency adj = List.sort $ Adj.getNodeSuccNodes node adj
	nodeIsInPreds adj = foldl (\ans (src, dst) -> ans || (src == node)) False (Adj.getAdjacencies adj)

-- | Remove all the node's pred adjacencies and check if they were removed with getAdjacencies.
prop_removeNodePreds :: [(Int, Int)] -> Int -> Bool
prop_removeNodePreds arcs node = (nodesPredsFromCreatedAdjacency adj == []) && (not $ nodeIsInSuccs adj) where
	adj = Adj.removeNodePredAdjacencies node $ addArcList arcs Adj.empty
	nodesPredsFromCreatedAdjacency adj = List.sort $ Adj.getNodePredNodes node adj
	nodeIsInSuccs adj = foldl (\ans (src, dst) -> ans || (dst == node)) False (Adj.getAdjacencies adj)

--prop_addAdjacency arcs = (Set.fromList nodes) == (Set.fromList $ getNodes $ foldl (\adj node -> addNode node adj) empty nodes)
--  where types = arcs :: [(Int, Int)]
