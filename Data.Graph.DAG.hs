module Data.Graph.DAG (
	DAG(),
--TODO	empty,
	generatesCycle) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Graph as Graph
import qualified Data.Graph.Digraph as Digraph

-- DATA DEFINITION
-------------------------------------------------------------------------------

--TODO: Use newtype instead of data when TmpDAG is removed
data DAG node edge = DAG (Digraph.Digraph node edge) | TmpDAG edge
	deriving (Show, Read, Ord, Eq)

-- CONSTRUCTORS
-------------------------------------------------------------------------------

--TODO empty :: Ord node => DAG (Digraph.Digraph node)
--TODO empty = DAG Digraph.empty

-- CLASS DEFINITION
-------------------------------------------------------------------------------

instance Graph.Graph DAG where

	addNode node (DAG digraph) = DAG (Graph.addNode node digraph)

	removeNode node (DAG digraph) = DAG (Graph.removeNode node digraph)

	addEdge tail head dag@(DAG digraph)
		| generatesCycle tail head dag = error ("Linking generates a cicle")
		| otherwise = DAG (Graph.addEdge tail head digraph)

	unlink tail head (DAG digraph) = DAG (Graph.unlink tail head digraph)

	nodes (DAG digraph) = Graph.nodes digraph

	edges (DAG digraph) = Graph.edges digraph 

	reachable node (DAG digraph) = Graph.reachable node digraph

	nodeEdges node (DAG digraph) = Graph.nodeEdges node digraph

-- UTILITIES
-------------------------------------------------------------------------------

-- Checks if the connection generates a cicle.
generatesCycle :: (Ord node, Ord edge) => node -> node -> DAG node edge -> Bool
generatesCycle tail head (DAG digraph) = Digraph.generatesCycle tail head digraph

