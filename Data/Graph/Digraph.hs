-- Author: Federico Mastellone (fmaste@gmail.com)

module Data.Graph.Digraph (
	Digraph(),
--TODO	empty,
	generatesCycle) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Graph as Graph
import qualified Data.Graph.Labels as Labels
import qualified Data.Graph.Adjacency as Adj

-- DATA DEFINITION
-------------------------------------------------------------------------------

type EdgeLabels node edge = Labels.Labels (node, node) edge

data Digraph node edge = Digraph (Adj.Adjacency node) (EdgeLabels node edge)
    deriving (Show, Read, Ord, Eq)

-- CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

empty :: (Ord node, Ord edge) => Digraph node edge
empty = Digraph Adj.empty Labels.empty

-- CLASS DEFINITION
-------------------------------------------------------------------------------

instance Graph.Graph Digraph where

	addNode node (Digraph adj labels) = Digraph adj' labels where
		adj' = Adj.addNode node adj

	removeNode node digraph@(Digraph adj labels) = Digraph adj' labels' where
		adj' = Adj.removeNode node adj
		labels' = foldl (\ans arc -> Labels.removeElement arc ans) labels $ Graph.nodeEdges node digraph

	addEdge src dst edge (Digraph adj labels) = Digraph adj' labels' where
		adj' = Adj.addAdjacency src dst adj
		labels' = Labels.addLabelToElement (src, dst) edge labels

	removeEdge src dst edge (Digraph adj labels) = Digraph adj' labels' where
		adj' = Adj.removeAdjacency src dst adj
		labels' = Labels.removeElementLabel (src, dst) edge labels

	getNodes (Digraph adj _) = Adj.getNodes adj

	getEdges (Digraph adj _) = Adj.getAdjacencies adj

	getNodeCount (Digraph adj _) = Adj.getNodeCount adj

	-- TODO: getEdgeCount

	containsNode node (Digraph adj _) = Adj.containsNode node adj

	-- TODO: containsEdge

	reachable node digraph = (heads node digraph) ++ (tails node digraph)

	nodeEdges node digraph = (headArcs node digraph) ++ (tailArcs node digraph)

-------------------------------------------------------------------------------


-- Checks if the connection generates a cicle.
generatesCycle :: (Ord node, Ord edge) => node -> node -> Digraph node edge -> Bool
generatesCycle tail head digraph = isParent [tail] where
	isParent [] = False
	isParent (x:xs) = (x == head) || (isParent xs) || (isParent (tails x digraph))

-- CONSTRUCTOR FUNCTIONS
-------------------------------------------------------------------------------
-- GETTER FUNCTIONS
-------------------------------------------------------------------------------

-- Gets a list of all the nodes with no predecessors.
roots :: (Ord node, Ord edge) => Digraph node edge -> [node]
roots (Digraph adj _) = filter (\x -> (Adj.getNodePredNodes x adj) == []) (Adj.getNodes adj)

-- Gets a list of all the nodes with no successors.
leafs :: (Ord node, Ord edge) => Digraph node edge -> [node]
leafs (Digraph adj _) = filter (\x -> (Adj.getNodeSuccNodes x adj) == []) (Adj.getNodes adj)

-- TODO: isolated (The nodes without connections)

-- NODE GETTER FUNCTIONS
-------------------------------------------------------------------------------

-- Gets a list with the direct successors of a node.
heads :: (Ord node, Ord edge) => node -> Digraph node edge -> [node]
heads node (Digraph adj _) = Adj.getNodeSuccNodes node adj

-- Generates a list of connection tuples (tail, head) with the direct successors of this node.
headArcs :: (Ord node, Ord edge) => node -> Digraph node edge -> [(node, node)]
headArcs node digraph = map (\a -> (node, a)) (heads node digraph)

-- Gets a list with the direct predecessors of a node.
tails :: (Ord node, Ord edge) => node -> Digraph node edge -> [node]
tails node (Digraph adj _) = Adj.getNodePredNodes node adj

-- Generates a list of connection tuples (tail, head) with the direct predecessors of this node.
tailArcs :: (Ord node, Ord edge) => node -> Digraph node edge -> [(node, node)]
tailArcs node digraph = map (\a -> (a, node)) (tails node digraph)

