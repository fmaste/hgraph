-- Author: Federico Mastellone (fmaste@gmail.com)

-- Module that describes a node and its inmediate relationships with other nodes.
-- There are two types of inmediate relationsips that a node can have:
-- Being a successor or a predecessor to another node.
-- Higher grade relationships are not handled by this module.
-- Also, if node a and b are connected in the same direction by two different edges, 
-- each one with different values, this module only tells that they are connected 
-- by an edge, but not which one. You can't distinguish edges.
-- In short, this module handles two-way inmediate relationships between things of the same type.
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
	revert,
	reconstruct) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.MultiMap as MM

-- * DATA DEFINITION
-------------------------------------------------------------------------------

-- | The adjacency of a graph here is defined with an incidence list with the 
-- direct node successors and another one with the direct predecessors.
-- This incidence lists only represent to which nodes a node is connected by 
-- a single edge. Being the difference between both lists the diretion of the 
-- edge, either "to" or "from" the node.
data Adjacency node = Adjacency (NodeSuccs node) (NodePreds node)
    deriving (Show, Read, Ord, Eq)

-- | The nodes that can be reached by a single edge from a given node.
-- A map of nodes as keys and a set of the nodes that are a direct successor of that node as value.
type NodeSuccs node = MM.MultiMap node node

-- | The nodes that can reach by a single edge a given node.
-- A map of nodes as keys and a set of the nodes that are a direct predecessor of that node as value.
type NodePreds node = MM.MultiMap node node

-- * CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- | The empty Adjacency.
empty :: Ord node => Adjacency node
empty = Adjacency (MM.empty) (MM.empty)

-- | Adds a node without any adjacencies.
-- If the node already exists the original Adjacency is returned.
addNode :: Ord node => node -> Adjacency node -> Adjacency node
addNode node (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = MM.addKey node succs
	preds' = MM.addKey node preds

-- | Removes the node and all the adjacencies were the node participates.
-- If the node does not exists the original Adjacency is returned.
removeNode :: Ord node => node -> Adjacency node -> Adjacency node
removeNode node adj = removeNode' $ removeNodeAdjacencies node adj where
	removeNode' (Adjacency succs preds) = Adjacency succs' preds' where
		succs' = MM.removeKey node succs
		preds' = MM.removeKey node preds
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
	succs' = MM.addKey dst $ MM.addValue src dst succs
	preds' = MM.addKey src $ MM.addValue dst src preds

-- | Removes the adjacency from src to dst.
-- If src or dst do not exist the original Adjacency is returned.
-- If the adjancecy does not exists the original Adjacency is returned.
removeAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
removeAdjacency src dst (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = MM.removeValue src dst succs
	preds' = MM.removeValue dst src preds

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
	succs' = MM.removeValuesAll node succs
	preds' = foldl removeSuccFromPreds preds (getNodeSuccNodes node adj) where
		removeSuccFromPreds predsMap predNode = MM.removeValue predNode node predsMap

-- | Removes all the adjacencies were this node is successor.
-- If node does not exists the original Adjacency is returned.
-- If there are no adjacencies were this node is successor the original Adjacency is returned.
removeNodePredAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodePredAdjacencies node adj@(Adjacency succs preds) = Adjacency succs' preds' where
	succs' = foldl removePredFromSuccs succs (getNodePredNodes node adj) where
		removePredFromSuccs succsMap succNode = MM.removeValue succNode node succsMap
	preds' = MM.removeValuesAll node preds

-- * QUERY FUNCTIONS
-------------------------------------------------------------------------------

-- | A list with all the different nodes.
getNodes :: Ord node => Adjacency node -> [node]
getNodes (Adjacency succs _) = MM.getKeys succs

-- | The number of different nodes present.
getNodeCount :: Ord node => Adjacency node -> Int
getNodeCount (Adjacency succs _) = MM.getKeyCount succs

-- | Get all the different nodes that are successors.
getNodeSuccNodes :: Ord node => node -> Adjacency node -> [node]
getNodeSuccNodes node (Adjacency succs _) = MM.getValues node succs

-- | Get all the different nodes that are predecessors.
getNodePredNodes :: Ord node => node -> Adjacency node -> [node]
getNodePredNodes node (Adjacency _ preds) = MM.getValues node preds

-- | A set with the node successors.
getNodeSuccNodesSet :: Ord node => node -> Adjacency node -> Set.Set node
getNodeSuccNodesSet node (Adjacency succs _) = MM.getValuesSet node succs

-- | A set with the node predecessors.
getNodePredNodesSet :: Ord node => node -> Adjacency node -> Set.Set node
getNodePredNodesSet node (Adjacency _ preds) = MM.getValuesSet node preds

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
	foldl (\ans key -> ans + (MM.getValueCount key succs)) 0 $ MM.getKeys succs

-- | Node exists?
containsNode :: Ord node => node -> Adjacency node -> Bool
containsNode node (Adjacency succs _) = MM.containsKey node succs

-- | Is this a successor of node?
containsNodeSucc :: Ord node => node -> node -> Adjacency node -> Bool
containsNodeSucc node succ adj = containsAdjacency node succ adj

-- | Is this a predecessor of node?
containsNodePred :: Ord node => node -> node -> Adjacency node -> Bool
containsNodePred node pred adj = containsAdjacency pred node adj

-- | Adjacency exists?
containsAdjacency :: Ord node => node -> node -> Adjacency node -> Bool
containsAdjacency src dst (Adjacency succs _) = MM.containsValue src dst succs

-- * CONVERSION
-------------------------------------------------------------------------------

-- | All connections are reverted, every src -> dst becomes src <- dst.
revert :: Ord node => Adjacency node -> Adjacency node
revert (Adjacency succs preds) = Adjacency preds succs where

-- * DEBUGGING
-------------------------------------------------------------------------------

-- Reconstruct succs using preds and viceversa.
-- Used to demostrate that both structures have the same info but in defferent formats.
reconstruct :: Ord node => Adjacency node -> Adjacency node
reconstruct (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = foldl f MM.empty $ MM.getKeys preds where
		-- TODO: The (MM.addKey key mm) should not be necessary, but QuickCheck fails without it!
		f mm key = foldl g (MM.addKey key mm) $ MM.getValues key preds where
			g mm pred = MM.addValue pred key mm 
	preds' = foldl f MM.empty $ MM.getKeys succs where
		-- TODO: The (MM.addKey key mm) should not be necessary, but QuickCheck fails without it!
		f mm key = foldl g (MM.addKey key mm) $ MM.getValues key succs where
			g mm succ = MM.addValue succ key mm 
