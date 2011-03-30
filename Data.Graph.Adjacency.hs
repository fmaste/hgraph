-- Author: Federico Mastellone (fmaste@gmail.com)

module Data.Graph.Adjacency (
	Adjacency(),
	empty,
	addNode,
	removeNode,
	addAdjacency,
	removeAdjacency,
	removeFullAdjacency,
	removeNodeAdjacencies,
	removeNodeSuccAdjacencies,
	removeNodePredAdjacencies,
	getNodes,
	getNodeCount,
	getNodeSuccNodes,
	getNodePredNodes,
	getNodeSuccNodesSet,
	getNodePredNodesSet,
	getNodeAdjacentNodes,
	getNodeAdjacentNodesSet,
	getNodeAdjacencies,
	getNodeSuccAdjacencies,
	getNodePredAdjacencies,
	getAdjacencies,
	getAdjacencyCount,
	containsNode,
	containsNodeSucc,
	containsNodePred,
	containsAdjacency,
	revert) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Test.QuickCheck as QC
import qualified Data.List as List

-- * DATA DEFINITION
-------------------------------------------------------------------------------

-- | The adjacency of a graph is defined with an incidence list with the node successors and another one with the predecessors.
-- This incidence lists only represent to which nodes a node is connected, but not how it is connected. For example: If node
-- a and b are connected by two different edges, each one with different values, this lists only tell you that a path exists 
-- between the two nodes, but nothing specific about that path(s).
data Adjacency node = Adjacency (NodeSuccs node) (NodePreds node)
    deriving (Show, Read, Ord, Eq)

-- | The nodes that can be reached from a given node.
-- A map of nodes as keys and a set of the nodes that are a direct successor of that node as value.
type NodeSuccs node = Map.Map node (Set.Set node)

-- | The nodes that can reach a given node.
-- A map of nodes as keys and a set of the nodes that are a direct predecessor of that node as value.
type NodePreds node = Map.Map node (Set.Set node)

-- * CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- | The empty Adjacency.
empty :: Ord node => Adjacency node
empty = Adjacency (Map.empty) (Map.empty)

-- | Adds a node without any adjacencies.
-- If the node already exists the original Adjacency is returned.
addNode :: Ord node => node -> Adjacency node -> Adjacency node
addNode node (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.insertWith (\new old -> old) node Set.empty succs
	preds' = Map.insertWith (\new old -> old) node Set.empty preds

-- | Removes the node and all the adjacencies were the node participates.
-- If the node does not exists the original Adjacency is returned.
removeNode :: Ord node => node -> Adjacency node -> Adjacency node
removeNode node adj = removeNode' $ removeNodeAdjacencies node adj where
	removeNode' (Adjacency succs preds) = Adjacency succs' preds' where
		succs' = Map.delete node succs
		preds' = Map.delete node preds
{- Can also be done as:
	removeNode node (Adjacency succs preds) = Adjacency succs' preds' where
		succs' = Map.delete node (Map.map (Set.delete node) succs)
		preds' = Map.delete node (Map.map (Set.delete node) preds)
	But this means interating through all the internal sets (can be very big)
	and I already know which sets because of the succ/preds indexes.
-}

-- | Adds an adjacency from src to dst.
-- If src or dst do not exist they are added.
-- If the adjacency already exists the original Adjacency is returned.
addAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
addAdjacency src dst (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.insertWith (\new old -> old) dst Set.empty $ Map.insertWith (\new old -> Set.insert dst old) src (Set.singleton dst) succs
	preds' = Map.insertWith (\new old -> old) src Set.empty $ Map.insertWith (\new old -> Set.insert src old) dst (Set.singleton src) preds

-- | Removes the adjacency from src to dst.
-- If src or dst do not exist the original Adjacency is returned.
-- If the adjancecy does not exists the original Adjacency is returned.
removeAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
removeAdjacency src dst (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.adjust (Set.delete dst) src succs
	preds' = Map.adjust (Set.delete src) dst preds

-- | Removes all the adjacencies between node1 and node2.
-- If node1 or node2 do not exist the original Adjacency is returned.
-- If no adjacencies between node1 and node2 exist the original Adjacency is returned.
removeFullAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
removeFullAdjacency node1 node2 adj = 
	removeAdjacency node1 node2 $ removeAdjacency node2 node1 adj

-- | Removes all the adjacencies were this node participates.
-- If node does not exists the original Adjacency is returned.
-- If there are no adjacencies were this node participates the original Adjacency is returned.
removeNodeAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodeAdjacencies node adj =
	removeNodeSuccAdjacencies node $ removeNodePredAdjacencies node adj

-- | Removes all the adjacencies were this node is predecessor.
-- If node does not exists the original Adjacency is returned.
-- If there are no adjacencies were this node is predecessor the original Adjacency is returned.
removeNodeSuccAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodeSuccAdjacencies node adj@(Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.adjust (const Set.empty) node succs
	preds' = foldl removeSuccFromPreds preds (getNodeSuccNodes node adj) where
		removeSuccFromPreds predsMap predNode = Map.adjust (Set.delete node) predNode predsMap

-- | Removes all the adjacencies were this node is successor.
-- If node does not exists the original Adjacency is returned.
-- If there are no adjacencies were this node is successor the original Adjacency is returned.
removeNodePredAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodePredAdjacencies node adj@(Adjacency succs preds) = Adjacency succs' preds' where
	succs' = foldl removePredFromSuccs succs (getNodePredNodes node adj) where
		removePredFromSuccs succsMap succNode = Map.adjust (Set.delete node) succNode succsMap
	preds' = Map.adjust (const Set.empty) node preds

-- * QUERY FUNCTIONS
-------------------------------------------------------------------------------

-- | A list with all the different nodes.
getNodes :: Ord node => Adjacency node -> [node]
getNodes (Adjacency succs _) = Map.keys succs

-- | The number of different nodes present.
getNodeCount :: Adjacency node -> Int
getNodeCount (Adjacency succs _) = Map.size succs

-- | Get all the different nodes that are successors.
getNodeSuccNodes :: Ord node => node -> Adjacency node -> [node]
getNodeSuccNodes node adj = Set.elems $ getNodeSuccNodesSet node adj

-- | Get all the different nodes that are predecessors.
getNodePredNodes :: Ord node => node -> Adjacency node -> [node]
getNodePredNodes node adj = Set.elems $ getNodePredNodesSet node adj

-- | A set with the node successors.
getNodeSuccNodesSet :: Ord node => node -> Adjacency node -> Set.Set node
getNodeSuccNodesSet node (Adjacency succs _) = 
	Map.findWithDefault Set.empty node succs

-- | A set with the node predecessors.
getNodePredNodesSet :: Ord node => node -> Adjacency node -> Set.Set node
getNodePredNodesSet node (Adjacency _ preds) = 
	Map.findWithDefault Set.empty node preds

-- | The different nodes that are adjacencent, either succs or preds.
getNodeAdjacentNodes :: Ord node => node -> Adjacency node -> [node]
getNodeAdjacentNodes node adj = 
	Set.elems $ getNodeAdjacentNodesSet node adj

-- | A set with the different nodes that are adjacencent, either succs or preds.
getNodeAdjacentNodesSet :: Ord node => node -> Adjacency node -> Set.Set node
getNodeAdjacentNodesSet node adj = 
	Set.union (getNodeSuccNodesSet node adj) (getNodePredNodesSet node adj)

-- | The different adjacencies were this node participates.
getNodeAdjacencies :: Ord node => node -> Adjacency node -> [(node, node)]
getNodeAdjacencies node adj = 
	getNodeSuccAdjacencies node adj ++ getNodePredAdjacencies node adj

-- | The different adjacencies were this node is predecessor.
getNodeSuccAdjacencies :: Ord node => node -> Adjacency node -> [(node, node)]
getNodeSuccAdjacencies node adj = [(node, x) | x <- getNodeSuccNodes node adj]

-- | The different adjacencies were this node is successor.
getNodePredAdjacencies :: Ord node => node -> Adjacency node -> [(node, node)]
getNodePredAdjacencies node adj = [(x, node) | x <- getNodePredNodes node adj]

-- | All the different adjacencies that exist.
getAdjacencies :: Ord node => Adjacency node -> [(node, node)]
getAdjacencies adj = 
	concatMap (\node -> [(node, x) | x <- getNodeSuccNodes node adj]) (getNodes adj)

-- | The number of different adjacencies.
getAdjacencyCount :: Ord node => Adjacency node -> Int
getAdjacencyCount (Adjacency succs _) = 
	Map.fold (\aSet count -> count + Set.size aSet) 0 succs

-- | Node exists?
containsNode :: Ord node => node -> Adjacency node -> Bool
containsNode node (Adjacency succs _) = Map.member node succs

-- | Is this a successor of node?
containsNodeSucc :: Ord node => node -> node -> Adjacency node -> Bool
containsNodeSucc node succ adj = containsAdjacency node succ adj

-- | Is this a predecessor of node?
containsNodePred :: Ord node => node -> node -> Adjacency node -> Bool
containsNodePred node pred adj = containsAdjacency pred node adj

-- | Adjacency exists?
containsAdjacency :: Ord node => node -> node -> Adjacency node -> Bool
containsAdjacency src dst adj = Set.member dst $ getNodeSuccNodesSet src adj

-- * CONVERSION
-------------------------------------------------------------------------------

-- | All connections are reverted, every src -> dst becomes src <- dst.
revert :: Ord node => Adjacency node -> Adjacency node
revert (Adjacency succs preds) = Adjacency preds succs where

-- * QUICKCHECK
-------------------------------------------------------------------------------

addNodeList :: Ord node => [node] -> Adjacency node -> Adjacency node
addNodeList nodes adj = foldl (\adj' node -> addNode node adj') adj nodes

removeNodeList :: Ord node => [node] -> Adjacency node -> Adjacency node
removeNodeList nodes adj = foldl (\adj' node -> removeNode node adj') adj nodes

-- Adds the nodes on the list of adjacencies.
addNodesOnArcList :: Ord node => [(node, node)] -> Adjacency node -> Adjacency node
addNodesOnArcList arcs adj = foldl (\adj' (src, dst) -> addNode src $ addNode dst adj') adj arcs

-- Adds the list of adjacencies.
addArcList :: Ord node => [(node, node)] -> Adjacency node -> Adjacency node
addArcList arcs adj = foldl (\adj' arc -> uncurry addAdjacency arc adj') adj arcs

-- Removes the list of adjacencies.
removeArcList :: Ord node => [(node, node)] -> Adjacency node -> Adjacency node
removeArcList arcs adj = foldl (\adj' arc -> uncurry removeAdjacency arc adj') adj arcs

-- Removes the list of adjacencies.
removeFullArcList :: Ord node => [(node, node)] -> Adjacency node -> Adjacency node
removeFullArcList arcs adj = foldl (\adj' arc -> uncurry removeFullAdjacency arc adj') adj arcs

-- Reconstruct succs using preds and viceversa.
-- Used to demostrate that both structures have the same info but in defferent formats.
reconstruct :: Ord node => Adjacency node -> Adjacency node
reconstruct (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.foldWithKey f Map.empty preds where
		f succNode predsSet succs'' = Set.fold g (Map.insertWith (\new old -> old) succNode Set.empty succs'') predsSet where
			g predNode succs''' = Map.insertWith (\new old -> Set.insert succNode old) predNode (Set.singleton succNode) succs'''
	preds' = Map.foldWithKey f Map.empty succs where
		f predNode succsSet preds'' = Set.fold g (Map.insertWith (\new old -> old) predNode Set.empty preds'') succsSet where
			g succNode preds''' = Map.insertWith (\new old -> Set.insert predNode old) succNode (Set.singleton predNode) preds'''

prop_addNode :: [Int] -> Bool
prop_addNode nodes = nonRepeatInsertedNodesList == nodesFromCreatedAdjacency where
	nonRepeatInsertedNodesList = List.sort $ List.nub nodes
	nodesFromCreatedAdjacency = List.sort $ getNodes $ addNodeList nodes empty

prop_containsNode :: [Int] -> Bool
prop_containsNode nodes = all id [ containsNode x adj | x <- nodes] where
	adj = addNodeList nodes empty

prop_removeNode :: [Int] -> Bool
prop_removeNode nodes = nodesFromCreatedAdjacency == [] where
	nodesFromCreatedAdjacency = List.sort $ getNodes $ removeNodeList nodes $ addNodeList nodes empty

prop_notContainsNode :: [Int] -> Bool
prop_notContainsNode nodes = not $ any id [ containsNode x adj | x <- nodes] where
	adj = removeNodeList nodes $ addNodeList nodes empty

prop_removeNodeFromEmpty :: [Int] -> Bool
prop_removeNodeFromEmpty nodes = nodesFromEmptyAdjacency == [] where
	nodesFromEmptyAdjacency = List.sort $ getNodes $ removeNodeList nodes empty

prop_addNodesAndAdjacency :: [(Int, Int)] -> Bool
prop_addNodesAndAdjacency arcs = nonRepeatInsertedAdjacenciesList == adjacenciesFromCreatedAdjacency where
	nonRepeatInsertedAdjacenciesList = List.sort $ List.nub arcs
	adjacenciesFromCreatedAdjacency = List.sort $ getAdjacencies $ addArcList arcs $ addNodesOnArcList arcs empty

prop_addAdjacency :: [(Int, Int)] -> Bool
prop_addAdjacency arcs = nonRepeatInsertedAdjacenciesList == adjacenciesFromCreatedAdjacency where
	nonRepeatInsertedAdjacenciesList = List.sort $ List.nub arcs
	adjacenciesFromCreatedAdjacency = List.sort $ getAdjacencies $ addArcList arcs empty

prop_containsAdjacency :: [(Int, Int)] -> Bool
prop_containsAdjacency arcs = all id [ containsAdjacency src dst adj | (src, dst) <- arcs] where
	adj = addArcList arcs empty

prop_containsNodeSucc :: [(Int, Int)] -> Bool
prop_containsNodeSucc arcs = all id [ containsNodeSucc src dst adj | (src, dst) <- arcs] where
	adj = addArcList arcs empty

prop_containsNodePred :: [(Int, Int)] -> Bool
prop_containsNodePred arcs = all id [ containsNodePred dst src adj | (src, dst) <- arcs] where
	adj = addArcList arcs empty

prop_removeAdjacency :: [(Int, Int)] -> Bool
prop_removeAdjacency adjacencies = adjacenciesFromCreatedAdjacency == [] where
	adjacenciesFromCreatedAdjacency = getAdjacencies $ removeArcList adjacencies $ addArcList adjacencies empty

prop_notContainsAdjacency :: [(Int, Int)] -> Bool
prop_notContainsAdjacency arcs = not $ any id [ containsAdjacency src dst adj | (src, dst) <- arcs] where
	adj = removeArcList arcs $ addArcList arcs empty

prop_notContainsNodeSucc :: [(Int, Int)] -> Bool
prop_notContainsNodeSucc arcs = not $ any id [ containsNodeSucc src dst adj | (src, dst) <- arcs] where
	adj = removeArcList arcs $ addArcList arcs empty

prop_notContainsNodePred :: [(Int, Int)] -> Bool
prop_notContainsNodePred arcs = not $ any id [ containsNodePred dst src adj | (src, dst) <- arcs] where
	adj = removeArcList arcs $ addArcList arcs empty

prop_removeFullAdjacency :: [(Int, Int)] -> Bool
prop_removeFullAdjacency adjacencies = adjacenciesFromCreatedAdjacency == [] where
	adjacenciesFromCreatedAdjacency = getAdjacencies $ removeFullArcList (revertAdjs adjacencies) $ addArcList adjacencies empty where
		revertAdjs adjs = foldl (\ans (src, dst) -> ans ++ [(dst, src)]) [] adjs

prop_notContainsFullAdjacency :: [(Int, Int)] -> Bool
prop_notContainsFullAdjacency arcs = not $ any id [ (containsAdjacency src dst adj) && (containsAdjacency dst src adj) | (src, dst) <- arcs] where
	adj = removeFullArcList arcs $ addArcList arcs empty

prop_structure :: [(Int, Int)] -> Bool
prop_structure arcs = adj == (reconstruct $ reconstruct $ adj) where
	adj = addArcList arcs empty

prop_removeNodeSuccs :: [(Int, Int)] -> Int -> Bool
prop_removeNodeSuccs arcs node = (nodesSuccsFromCreatedAdjacency adj == []) && (not $ nodeIsInPreds adj) where
	adj = removeNodeSuccAdjacencies node $ addArcList arcs empty
	nodesSuccsFromCreatedAdjacency adj = List.sort $ getNodeSuccNodes node adj
	nodeIsInPreds (Adjacency _ preds) = Map.fold (\predsSet ans -> ans || (Set.member node predsSet)) False preds

prop_removeNodePreds :: [(Int, Int)] -> Int -> Bool
prop_removeNodePreds arcs node = (nodesPredsFromCreatedAdjacency adj == []) && (not $ nodeIsInSuccs adj) where
	adj = removeNodePredAdjacencies node $ addArcList arcs empty
	nodesPredsFromCreatedAdjacency adj = List.sort $ getNodePredNodes node adj
	nodeIsInSuccs (Adjacency succs _) = Map.fold (\succsSet ans -> ans || (Set.member node succsSet)) False succs

--prop_addAdjacency arcs = (Set.fromList nodes) == (Set.fromList $ getNodes $ foldl (\adj node -> addNode node adj) empty nodes)
--  where types = arcs :: [(Int, Int)]
