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

newtype DAG node = DAG (Digraph.Digraph node)
	deriving (Show, Read, Ord, Eq)

-- CONSTRUCTORS
-------------------------------------------------------------------------------

--TODO empty :: Ord node => DAG (Digraph.Digraph node)
--TODO empty = DAG Digraph.empty

-- CLASS DEFINITION
-------------------------------------------------------------------------------

instance Graph.Graph DAG where

	insert node (DAG digraph) = DAG (Graph.insert node digraph)

	delete node (DAG digraph) = DAG (Graph.delete node digraph)

	link tail head dag@(DAG digraph)
		| generatesCycle tail head dag = error ("Linking generates a cicle")
		| otherwise = DAG (Graph.link tail head digraph)

	unlink tail head (DAG digraph) = DAG (Graph.unlink tail head digraph)

	nodes (DAG digraph) = Graph.nodes digraph

	edges (DAG digraph) = Graph.edges digraph 

	reachable node (DAG digraph) = Graph.reachable node digraph

	nodeEdges node (DAG digraph) = Graph.nodeEdges node digraph

-- UTILITIES
-------------------------------------------------------------------------------

-- Checks if the connection generates a cicle.
generatesCycle :: Ord node => node -> node -> DAG node -> Bool
generatesCycle tail head (DAG digraph) = Digraph.generatesCycle tail head digraph

