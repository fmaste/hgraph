module Data.Graph.Digraph (
	Digraph(),
--TODO	empty,
	generatesCycle) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Graph as Graph
import qualified Data.Map as Map

-- DATA DEFINITION
-------------------------------------------------------------------------------
-- Using two adjacency lists to represent the digraph.

-- A map of nodes as keys and a list of the direct successors of that node as values.
type NodeSuccs node = (Map.Map node [node])

-- A map of nodes as keys and a list of the direct predecessors of that node as values.
type NodePreds node = (Map.Map node [node])

data Digraph node edge = Digraph (NodeSuccs node) (NodePreds node) | TmpDigraph edge
    deriving (Show, Read, Ord, Eq)

-- CONSTRUCTORS
-------------------------------------------------------------------------------

--TODO empty :: (Ord node, Ord edge) => Digraph node edge
--TODO empty = Digraph (Map.empty) (Map.empty) edge

-- CLASS DEFINITION
-------------------------------------------------------------------------------

instance Graph.Graph Digraph where

	addNode node (Digraph nodeSuccs nodePreds) = Digraph nodeSuccs' nodePreds' where
		nodeSuccs' = Map.insert node [] nodeSuccs
		nodePreds' = Map.insert node [] nodePreds

	removeNode node digraph = deleteFromMap (unlinkAll node digraph) where
		deleteFromMap (Digraph nodeSuccs nodePreds) = Digraph (Map.delete node nodeSuccs) (Map.delete node nodePreds)

	addEdge tail head digraph@(Digraph nodeSuccs nodePreds) = Digraph (addToMap tail head nodeSuccs) (addToMap head tail nodePreds) where
		addToMap src dest aMap = Map.insert src (dest:(aMap Map.! src)) aMap

	removeEdge tail head digraph@(Digraph nodeSuccs nodePreds) = Digraph (removeFromMap tail head nodeSuccs) (removeFromMap head tail nodePreds) where
		removeFromMap src dest aMap = Map.insert src (filter (\x -> x /= dest) (aMap Map.! src)) aMap

	getNodes (Digraph nodeSuccs _) = Map.keys nodeSuccs

	getEdges digraph = concatMap (\a -> headArcs a digraph) (Graph.getNodes digraph)

	-- TODO: getNodeCount

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

-- Removes all the links between the node and its direct successors.
unlinkSuccessors :: (Ord node, Ord edge) => node -> Digraph node edge -> Digraph node edge
unlinkSuccessors node digraph = foldl (\a b -> Graph.removeEdge node b a) digraph (heads node digraph)

-- Removes all the links between the node and its direct predecessors.
unlinkPredecessors :: (Ord node, Ord edge) => node -> Digraph node edge -> Digraph node edge
unlinkPredecessors node digraph = foldl (\a b -> Graph.removeEdge b node a) digraph (tails node digraph)

-- Removes all the links that this node has.
unlinkAll :: (Ord node, Ord edge) => node -> Digraph node edge -> Digraph node edge
unlinkAll node digraph = unlinkSuccessors node (unlinkPredecessors node digraph)

-- GETTER FUNCTIONS
-------------------------------------------------------------------------------

-- Gets a list of all the nodes with no predecessors.
roots :: (Ord node, Ord edge) => Digraph node edge -> [node]
roots (Digraph _ nodePreds) = Map.keys (Map.filter (\xs -> xs == []) nodePreds)

-- Gets a list of all the nodes with no successors.
leafs :: (Ord node, Ord edge) => Digraph node edge -> [node]
leafs (Digraph nodeSuccs _) = Map.keys (Map.filter (\xs -> xs == []) nodeSuccs)

-- TODO: isolated (The nodes without connections)

-- NODE GETTER FUNCTIONS
-------------------------------------------------------------------------------

-- Gets a list with the direct successors of a node.
heads :: (Ord node, Ord edge) => node -> Digraph node edge -> [node]
heads node (Digraph nodeSuccs _) = nodeSuccs Map.! node

-- Generates a list of connection tuples (tail, head) with the direct successors of this node.
headArcs :: (Ord node, Ord edge) => node -> Digraph node edge -> [(node, node)]
headArcs node digraph = map (\a -> (node, a)) (heads node digraph)

-- Gets a list with the direct predecessors of a node.
tails :: (Ord node, Ord edge) => node -> Digraph node edge -> [node]
tails node (Digraph _ nodePreds) = nodePreds Map.! node

-- Generates a list of connection tuples (tail, head) with the direct predecessors of this node.
tailArcs :: (Ord node, Ord edge) => node -> Digraph node edge -> [(node, node)]
tailArcs node digraph = map (\a -> (a, node)) (tails node digraph)

