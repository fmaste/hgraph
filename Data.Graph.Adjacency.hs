-- Author: Federico Mastellone (fmaste@gmail.com)

module Data.Graph.Adjacency (
	Adjacency(),
	empty,
	addNode,
	removeNode,
	addAdjacency,
	removeAdjacency,
	removeFullAdjacency,
	removeArc,
	removeNodeAdjacencies,
	removeNodeSuccAdjacencies,
	removeNodePredAdjacencies,
	getNodes,
	getNodeCount,
	getNodeSuccs,
	getNodePreds,
	getNodeSuccsArcs,
	getNodePredsArcs,
	getNodeSuccsSet,
	getNodePredsSet,
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

-- CONSTRUCTION
-------------------------------------------------------------------------------

-- The empty Adjacency
empty :: Ord node => Adjacency node
empty = Adjacency (Map.empty) (Map.empty)

-- If the node already exists, nothing happens.
addNode :: Ord node => node -> Adjacency node -> Adjacency node
addNode node (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.insertWith (\new old -> old) node Set.empty succs
	preds' = Map.insertWith (\new old -> old) node Set.empty preds

-- TODO:
-- addNodeSuccs :: Ord node => node -> [node] -> Adjacency node -> Adjacency node
-- addNodePreds :: Ord node => node -> [node] -> Adjacency node -> Adjacency node

-- Also removes all the node adjacencies.
{- Can also be done as:
	removeNode node (Adjacency succs preds) = Adjacency succs' preds' where
		succs' = Map.delete node (Map.map (Set.delete node) succs)
		preds' = Map.delete node (Map.map (Set.delete node) preds)
	But this means interating through all the internal sets (can be very big)
	and I already know which sets because of the succ/preds indexes.
-}
removeNode :: Ord node => node -> Adjacency node -> Adjacency node
removeNode node adj = removeNode' $ removeNodeAdjacencies node adj where
	removeNode' (Adjacency succs preds) = Adjacency succs' preds' where
		succs' = Map.delete node succs
		preds' = Map.delete node preds

addAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
addAdjacency src dst (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.adjust (Set.insert dst) src succs
	preds' = Map.adjust (Set.insert src) dst preds

removeAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
removeAdjacency src dst (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.adjust (Set.delete dst) src succs
	preds' = Map.adjust (Set.delete src) dst preds

removeFullAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
removeFullAdjacency node1 node2 adj = 
	removeAdjacency node1 node2 $ removeAdjacency node2 node1 adj

removeArc :: Ord node => (node, node) -> Adjacency node -> Adjacency node
removeArc (src, dst) adj = removeAdjacency src dst adj

removeNodeAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodeAdjacencies node adj =
	removeNodeSuccAdjacencies node $ removeNodePredAdjacencies node adj

removeNodeSuccAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodeSuccAdjacencies node adj@(Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.adjust (const Set.empty) node succs
	preds' = foldl removeSuccFromPreds preds (getNodeSuccs node adj) where
		removeSuccFromPreds predsMap predNode = Map.adjust (Set.delete node) predNode predsMap

removeNodePredAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodePredAdjacencies node adj@(Adjacency succs preds) = Adjacency succs' preds' where
	succs' = foldl removePredFromSuccs succs (getNodePreds node adj) where
		removePredFromSuccs succsMap succNode = Map.adjust (Set.delete node) succNode succsMap
	preds' = Map.adjust (const Set.empty) node preds

-- TODO
-- removeNodeSelectedAdjacencies :: Ord node => node -> [node] -> Adjacency node -> Adjacency node

-- QUERY
-------------------------------------------------------------------------------

getNodes :: Ord node => Adjacency node -> [node]
getNodes (Adjacency succs _) = Map.keys succs

getNodeCount :: Adjacency node -> Int
getNodeCount (Adjacency succs _) = Map.size succs

getNodeSuccs :: Ord node => node -> Adjacency node -> [node]
getNodeSuccs node adj = Set.elems $ getNodeSuccsSet node adj

getNodePreds :: Ord node => node -> Adjacency node -> [node]
getNodePreds node adj = Set.elems $ getNodePredsSet node adj

getNodeSuccsArcs :: Ord node => node -> Adjacency node -> [(node, node)]
getNodeSuccsArcs node adj = [(node, x) | x <- getNodeSuccs node adj]

getNodePredsArcs :: Ord node => node -> Adjacency node -> [(node, node)]
getNodePredsArcs node adj = [(x, node) | x <- getNodePreds node adj]

getNodeSuccsSet :: Ord node => node -> Adjacency node -> Set.Set node
getNodeSuccsSet node (Adjacency succs _) = 
	Map.findWithDefault Set.empty node succs

getNodePredsSet :: Ord node => node -> Adjacency node -> Set.Set node
getNodePredsSet node (Adjacency _ preds) = 
	Map.findWithDefault Set.empty node preds

getAdjacencies :: Ord node => Adjacency node -> [(node, node)]
getAdjacencies adj = 
	concatMap (\node -> [(node, x) | x <- getNodeSuccs node adj]) (getNodes adj)

getAdjacencyCount :: Ord node => Adjacency node -> Int
getAdjacencyCount (Adjacency succs _) = 
	Map.fold (\aSet count -> count + Set.size aSet) 0 succs

getNodeAdjacencies :: Ord node => node -> Adjacency node -> [(node, node)]
getNodeAdjacencies node adj = 
	getNodeSuccsArcs node adj ++ getNodePredsArcs node adj

containsNode :: Ord node => node -> Adjacency node -> Bool
containsNode node (Adjacency succs _) = Map.member node succs

containsNodeSucc :: Ord node => node -> node -> Adjacency node -> Bool
containsNodeSucc node succ adj = containsAdjacency node succ adj

containsNodePred :: Ord node => node -> node -> Adjacency node -> Bool
containsNodePred node pred adj = containsAdjacency pred node adj

containsAdjacency :: Ord node => node -> node -> Adjacency node -> Bool
containsAdjacency src dst adj = Set.member dst $ getNodeSuccsSet src adj
