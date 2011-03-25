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
	getNodeSuccs,
	getNodePreds,
	getNodeSuccsSet,
	getNodePredsSet,
	getNodeSuccAdjacencies,
	getNodePredAdjacencies,
	getAdjacencies,
	getAdjacencyCount,
	getNodeAdjacencies,
	containsNode,
	containsNodeSucc,
	containsNodePred,
	containsAdjacency) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Data.Set as Set

-- DATA DEFINITION
-------------------------------------------------------------------------------

-- The adjacency of a graph is defined with an incidence list with the node successors and another one with the predecessors.
-- This incidence lists only represent to which nodes a node is connected, but not how it is connected. For example: If node
-- a and b are connected by two different edges, each one with different values, this lists only tell you that a path exists 
-- between the two nodes, but nothing especific about that path(s).
data Adjacency node = Adjacency (NodeSuccs node) (NodePreds node)
    deriving (Show, Read, Ord, Eq)

-- The nodes that can be reached from a given node.
-- A map of nodes as keys and a set of the nodes that are a direct successor of that node as value.
type NodeSuccs node = Map.Map node (Set.Set node)

-- The nodes that can reach a given node.
-- A map of nodes as keys and a set of the nodes that are a direct predecessor of that node as value.
type NodePreds node = Map.Map node (Set.Set node)

-- CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- The empty Adjacency.
empty :: Ord node => Adjacency node
empty = Adjacency (Map.empty) (Map.empty)

-- Adds a node without any adjacencies.
-- If the node already exists the original Adjacency is returned.
addNode :: Ord node => node -> Adjacency node -> Adjacency node
addNode node (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.insertWith (\new old -> old) node Set.empty succs
	preds' = Map.insertWith (\new old -> old) node Set.empty preds

-- Removes the node and all the adjacencies were the node participates.
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

-- Adds an adjacency from src to dst.
-- If src or dst do not exist they are added.
-- If the adjacency already exists the original Adjacency is returned.
addAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
addAdjacency src dst (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.insertWith (\new old -> old) dst Set.empty $ Map.insertWith (\new old -> Set.insert dst old) src (Set.singleton dst) succs
	preds' = Map.insertWith (\new old -> old) src Set.empty $ Map.insertWith (\new old -> Set.insert src old) dst (Set.singleton src) preds

-- Removes the adjacency from src to dst.
-- If src or dst do not exist the original Adjacency is returned.
-- If the adjancecy does not exists the original Adjacency is returned.
removeAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
removeAdjacency src dst (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.adjust (Set.delete dst) src succs
	preds' = Map.adjust (Set.delete src) dst preds

-- Removes all the adjacencies between node1 and node2.
-- If node1 or node2 do not exist the original Adjacency is returned.
-- If no adjacencies between node1 and node2 exist the original Adjacency is returned.
removeFullAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
removeFullAdjacency node1 node2 adj = 
	removeAdjacency node1 node2 $ removeAdjacency node2 node1 adj

-- Removes all the adjacencies were this node participates.
-- If node does not exists the original Adjacency is returned.
-- If there are no adjacencies were this node participates the original Adjacency is returned.
removeNodeAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodeAdjacencies node adj =
	removeNodeSuccAdjacencies node $ removeNodePredAdjacencies node adj

-- Removes all the adjacencies were this node is predecessor.
-- If node does not exists the original Adjacency is returned.
-- If there are no adjacencies were this node is predecessor the original Adjacency is returned.
removeNodeSuccAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodeSuccAdjacencies node adj@(Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.adjust (const Set.empty) node succs
	preds' = foldl removeSuccFromPreds preds (getNodeSuccs node adj) where
		removeSuccFromPreds predsMap predNode = Map.adjust (Set.delete node) predNode predsMap

-- Removes all the adjacencies were this node is successor.
-- If node does not exists the original Adjacency is returned.
-- If there are no adjacencies were this node is successor the original Adjacency is returned.
removeNodePredAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodePredAdjacencies node adj@(Adjacency succs preds) = Adjacency succs' preds' where
	succs' = foldl removePredFromSuccs succs (getNodePreds node adj) where
		removePredFromSuccs succsMap succNode = Map.adjust (Set.delete node) succNode succsMap
	preds' = Map.adjust (const Set.empty) node preds

-- QUERY FUNCTIONS
-------------------------------------------------------------------------------

-- A list with all the different nodes.
getNodes :: Ord node => Adjacency node -> [node]
getNodes (Adjacency succs _) = Map.keys succs

-- The number of different nodes present.
getNodeCount :: Adjacency node -> Int
getNodeCount (Adjacency succs _) = Map.size succs

-- Get all the different nodes that are successors.
getNodeSuccs :: Ord node => node -> Adjacency node -> [node]
getNodeSuccs node adj = Set.elems $ getNodeSuccsSet node adj

-- Get all the different nodes that are predecessors.
getNodePreds :: Ord node => node -> Adjacency node -> [node]
getNodePreds node adj = Set.elems $ getNodePredsSet node adj

-- A set with the node successors.
getNodeSuccsSet :: Ord node => node -> Adjacency node -> Set.Set node
getNodeSuccsSet node (Adjacency succs _) = 
	Map.findWithDefault Set.empty node succs

-- A set with the node predecessors.
getNodePredsSet :: Ord node => node -> Adjacency node -> Set.Set node
getNodePredsSet node (Adjacency _ preds) = 
	Map.findWithDefault Set.empty node preds

-- The adjacencies were this node is predecessor.
getNodeSuccAdjacencies :: Ord node => node -> Adjacency node -> [(node, node)]
getNodeSuccAdjacencies node adj = [(node, x) | x <- getNodeSuccs node adj]

-- The adjacencies were this node is successor.
getNodePredAdjacencies :: Ord node => node -> Adjacency node -> [(node, node)]
getNodePredAdjacencies node adj = [(x, node) | x <- getNodePreds node adj]

-- All the different adjacencies that exist.
getAdjacencies :: Ord node => Adjacency node -> [(node, node)]
getAdjacencies adj = 
	concatMap (\node -> [(node, x) | x <- getNodeSuccs node adj]) (getNodes adj)

-- The number of different adjacencies.
getAdjacencyCount :: Ord node => Adjacency node -> Int
getAdjacencyCount (Adjacency succs _) = 
	Map.fold (\aSet count -> count + Set.size aSet) 0 succs

-- The node's different adjacencies.
getNodeAdjacencies :: Ord node => node -> Adjacency node -> [(node, node)]
getNodeAdjacencies node adj = 
	getNodeSuccAdjacencies node adj ++ getNodePredAdjacencies node adj

-- Node exists?
containsNode :: Ord node => node -> Adjacency node -> Bool
containsNode node (Adjacency succs _) = Map.member node succs

-- Is this a successor of node?
containsNodeSucc :: Ord node => node -> node -> Adjacency node -> Bool
containsNodeSucc node succ adj = containsAdjacency node succ adj

-- Is this a predecessor of node?
containsNodePred :: Ord node => node -> node -> Adjacency node -> Bool
containsNodePred node pred adj = containsAdjacency pred node adj

-- Adjacency exists?
containsAdjacency :: Ord node => node -> node -> Adjacency node -> Bool
containsAdjacency src dst adj = Set.member dst $ getNodeSuccsSet src adj
