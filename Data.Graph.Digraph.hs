module Data.Graph.Digraph (
	Digraph(),
--TODO	empty,
	generatesCycle) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Graph as Graph
import qualified Data.Graph.Adjacency as Adj
import qualified Data.Map as Map

-- DATA DEFINITION
-------------------------------------------------------------------------------

data Digraph node edge = Digraph (Adj.Adjacency node)
    deriving (Show, Read, Ord, Eq)

-- CONSTRUCTORS
-------------------------------------------------------------------------------

--TODO empty :: (Ord node, Ord edge) => Digraph node edge
--TODO empty = Digraph (Map.empty) (Map.empty) edge

-- CLASS DEFINITION
-------------------------------------------------------------------------------

instance Graph.Graph Digraph where

	addNode node (Digraph adj) = Digraph adj' where
		adj' = Adj.addNode node adj

	removeNode node (Digraph adj) = Digraph adj' where
		adj' = Adj.removeNode node adj

	addEdge src dst (Digraph adj) = Digraph adj' where
		adj' = Adj.addAdjacency src dst adj

	removeEdge src dst (Digraph adj) = Digraph adj' where
		adj' = Adj.removeAdjacency src dst adj

	getNodes (Digraph adj) = Adj.getNodes adj

	getEdges (Digraph adj) = Adj.getAdjacencies adj

	getNodeCount (Digraph adj) = Adj.getNodeCount adj

	-- TODO: getEdgeCount

	containsNode node (Digraph adj) = Adj.containsNode node adj

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
roots (Digraph adj) = filter (\x -> (Adj.getNodePreds x adj) == []) (Adj.getNodes adj)

-- Gets a list of all the nodes with no successors.
leafs :: (Ord node, Ord edge) => Digraph node edge -> [node]
leafs (Digraph adj) = filter (\x -> (Adj.getNodeSuccs x adj) == []) (Adj.getNodes adj)

-- TODO: isolated (The nodes without connections)

-- NODE GETTER FUNCTIONS
-------------------------------------------------------------------------------

-- Gets a list with the direct successors of a node.
heads :: (Ord node, Ord edge) => node -> Digraph node edge -> [node]
heads node (Digraph adj) = Adj.getNodeSuccs node adj

-- Generates a list of connection tuples (tail, head) with the direct successors of this node.
headArcs :: (Ord node, Ord edge) => node -> Digraph node edge -> [(node, node)]
headArcs node digraph = map (\a -> (node, a)) (heads node digraph)

-- Gets a list with the direct predecessors of a node.
tails :: (Ord node, Ord edge) => node -> Digraph node edge -> [node]
tails node (Digraph adj) = Adj.getNodePreds node adj

-- Generates a list of connection tuples (tail, head) with the direct predecessors of this node.
tailArcs :: (Ord node, Ord edge) => node -> Digraph node edge -> [(node, node)]
tailArcs node digraph = map (\a -> (a, node)) (tails node digraph)

