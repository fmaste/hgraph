module Data.Graph.Digraph (
	Digraph(),
	empty,
	generatesCycle) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Graph as Graph
import qualified Data.Map as Map

-- DATA DEFINITION
-------------------------------------------------------------------------------
-- Using two adjacency lists to represent the digraph.

-- A map of nodes as keys and a list of the direct successors of that node as values.
type TailOf node = (Map.Map node [node])

-- A map of nodes as keys and a list of the direct predecessors of that node as values.
type HeadOf node = (Map.Map node [node])

data Digraph node = Digraph (TailOf node) (HeadOf node)
    deriving (Show, Read, Ord, Eq)

-- CONSTRUCTORS
-------------------------------------------------------------------------------

empty :: Ord node => Digraph node
empty = Digraph Map.empty Map.empty

-- CLASS DEFINITION
-------------------------------------------------------------------------------

instance Graph.Graph Digraph where

	insert node (Digraph tailOf headOf) = Digraph (Map.insert node [] tailOf) (Map.insert node [] headOf)

	delete node digraph = deleteFromMap (unlinkAll node digraph) where
		deleteFromMap (Digraph tailOf headOf) = Digraph (Map.delete node tailOf) (Map.delete node headOf)

	link tail head digraph@(Digraph tailOf headOf) = Digraph (addToMap tail head tailOf) (addToMap head tail headOf) where
		addToMap src dest aMap = Map.insert src (dest:(aMap Map.! src)) aMap

	unlink tail head digraph@(Digraph tailOf headOf) = Digraph (removeFromMap tail head tailOf) (removeFromMap head tail headOf) where
		removeFromMap src dest aMap = Map.insert src (filter (\x -> x /= dest) (aMap Map.! src)) aMap

	nodes (Digraph tailOf _) = Map.keys tailOf

	edges digraph = concatMap (\a -> headArcs a digraph) (Graph.nodes digraph)

	reachable node digraph = (heads node digraph) ++ (tails node digraph)

	nodeEdges node digraph = (headArcs node digraph) ++ (tailArcs node digraph)

-------------------------------------------------------------------------------

-- Checks if the connection generates a cicle.
generatesCycle :: Ord node => node -> node -> Digraph node -> Bool
generatesCycle tail head digraph = isParent [tail] where
	isParent [] = False
	isParent (x:xs) = (x == head) || (isParent xs) || (isParent (tails x digraph))

-- CONSTRUCTOR FUNCTIONS
-------------------------------------------------------------------------------

-- Removes all the links between the node and its direct successors.
unlinkSuccessors :: Ord node => node -> Digraph node -> Digraph node
unlinkSuccessors node digraph = foldl (\a b -> Graph.unlink node b a) digraph (heads node digraph)

-- Removes all the links between the node and its direct predecessors.
unlinkPredecessors :: Ord node => node -> Digraph node -> Digraph node
unlinkPredecessors node digraph = foldl (\a b -> Graph.unlink b node a) digraph (tails node digraph)

-- Removes all the links that this node has.
unlinkAll :: Ord node => node -> Digraph node -> Digraph node
unlinkAll node digraph = unlinkSuccessors node (unlinkPredecessors node digraph)

-- GETTER FUNCTIONS
-------------------------------------------------------------------------------

-- Gets a list of all the nodes with no predecessors.
roots :: Ord node => Digraph node -> [node]
roots (Digraph _ headOf) = Map.keys (Map.filter (\xs -> xs == []) headOf)

-- Gets a list of all the nodes with no successors.
leafs :: Ord node => Digraph node -> [node]
leafs (Digraph tailOf _) = Map.keys (Map.filter (\xs -> xs == []) tailOf)

-- TODO: isolated (The nodes without connections)

-- NODE GETTER FUNCTIONS
-------------------------------------------------------------------------------

-- Gets a list with the direct successors of a node.
heads :: Ord node => node -> Digraph node -> [node]
heads node (Digraph tailOf _) = tailOf Map.! node

-- Generates a list of connection tuples (tail, head) with the direct successors of this node.
headArcs :: Ord node => node -> Digraph node -> [(node, node)]
headArcs node digraph = map (\a -> (node, a)) (heads node digraph)

-- Gets a list with the direct predecessors of a node.
tails :: Ord node => node -> Digraph node -> [node]
tails node (Digraph _ headOf) = headOf Map.! node

-- Generates a list of connection tuples (tail, head) with the direct predecessors of this node.
tailArcs :: Ord node => node -> Digraph node -> [(node, node)]
tailArcs node digraph = map (\a -> (a, node)) (tails node digraph)

