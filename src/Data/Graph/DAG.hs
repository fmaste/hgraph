-- Author: Federico Mastellone (fmaste@gmail.com)

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
data DAG node edge = DAG (Digraph.Digraph node edge)
	deriving (Show, Read, Ord, Eq)

-- CLASS DEFINITION
-------------------------------------------------------------------------------

instance Graph.Graph DAG where

	empty = (DAG Graph.empty)

	addNode node (DAG digraph) = DAG (Graph.addNode node digraph)

	removeNode node (DAG digraph) = DAG (Graph.removeNode node digraph)

	addEdge tail head dag@(DAG digraph)
		| generatesCycle tail head dag = error ("Linking generates a cicle")
		| otherwise = DAG (Graph.addEdge tail head digraph)

	removeEdge tail head (DAG digraph) = DAG (Graph.removeEdge tail head digraph)

	getNodes (DAG digraph) = Graph.getNodes digraph

	getEdges (DAG digraph) = Graph.getEdges digraph 

	reachable node (DAG digraph) = Graph.reachable node digraph

	nodeEdges node (DAG digraph) = Graph.nodeEdges node digraph

-- UTILITIES
-------------------------------------------------------------------------------

-- Checks if the connection generates a cicle.
generatesCycle :: (Ord node, Ord edge) => node -> node -> DAG node edge -> Bool
generatesCycle tail head (DAG digraph) = Digraph.generatesCycle tail head digraph

