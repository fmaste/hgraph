module Data.Graph.Adjacency (
	Adjacency(),
	empty,
	addNode,
	removeNode,
	addAdjacency,
	removeAdjacency,
	getNodes,
	getNodeCount,
	getAdjacencies,
	getNodeAdjacencies,
	getNodeSuccs,
	getNodePreds,
	containsNode,
	containsAdjacency) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Map as Map

-- DATA DEFINITION
-------------------------------------------------------------------------------

-- The adjacency of a graph is defined with an incidence list with the node successors and another one with the predecessors.
-- This incidence lists only represent to which nodes a node is connected, but not how it is connected. For example: If node
-- a and b are connected by two different edges, each one with different values, this lists only tell you that a path exists 
-- between the two nodes, but nothing especific about that path(s).
data Adjacency node = Adjacency (NodeSuccs node) (NodePreds node)
    deriving (Show, Read, Ord, Eq)

-- The nodes that can be reached from a given node.
-- A map of nodes as keys and a list of the nodes that are a direct successor of that node as value.
type NodeSuccs node = Map.Map node [node] -- TODO: Use a List instead of [] to make more performant adding or removing elements.

-- The nodes that can reach a given node.
-- A map of nodes as keys and a list of the nodes that are a direct predecessor of that node as value.
type NodePreds node = Map.Map node [node] -- TODO: Use a List instead of [] to make more performant adding or removing elements.

-- CONSTRUCTORS
-------------------------------------------------------------------------------

empty :: Ord node => Adjacency node
empty = Adjacency (Map.empty) (Map.empty)

-- FUNCTIONS
-------------------------------------------------------------------------------

-- If the node list already exists it is emptied.
addNode :: Ord node => node -> Adjacency node -> Adjacency node
addNode node (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.insert node [] succs
	preds' = Map.insert node [] preds

-- The appearences of the node on other lists are not modified.
removeNode :: Ord node => node -> Adjacency node -> Adjacency node
removeNode node adj@(Adjacency succs preds) = Adjacency succs' preds' where
		-- Fold through the predecessors to know which successors to adjust and them delete the node.
		succs' = Map.delete node (foldl (\aMap aNode -> Map.adjust (dropElem node) aNode aMap) succs (getNodePreds node adj))
		-- Fold through the successors to know which predecessors to adjust and them delete the node.
		preds' = Map.delete node (foldl (\aMap aNode -> Map.adjust (dropElem node) aNode aMap) preds (getNodeSuccs node adj))

addAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
addAdjacency src dst (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.adjust (\xs -> dst : (dropElem dst xs)) src succs -- First, if exists, the element is removed from the list.
	preds' = Map.adjust (\xs -> src : (dropElem src xs)) dst preds -- First, if exists, the element is removed from the list.

removeAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
removeAdjacency src dst (Adjacency succs preds) = Adjacency succs' preds' where
	succs' = Map.adjust (dropElem dst) src succs
	preds' = Map.adjust (dropElem dst) src preds

getNodes :: Ord node => Adjacency node -> [node]
getNodes (Adjacency succs _) = Map.keys succs

getNodeCount :: Adjacency node -> Int
getNodeCount (Adjacency succs _) = Map.size succs

getAdjacencies :: Ord node => Adjacency node -> [(node, node)]
getAdjacencies adj = concatMap (\node -> [(node, x) | x <- getNodeSuccs node adj]) (getNodes adj)

-- There is no getAdjacenciesCount because there is only one "edge" between two nodes, no matter how many times you add the adjacency.

getNodeAdjacencies :: Ord node => node -> Adjacency node -> [(node, node)]
getNodeAdjacencies node adj = [(node, x) | x <- getNodeSuccs node adj] ++ [(x, node) | x <- getNodePreds node adj]

getNodeSuccs :: Ord node => node -> Adjacency node -> [node]
getNodeSuccs node (Adjacency succs _) = succs Map.! node

getNodePreds :: Ord node => node -> Adjacency node -> [node]
getNodePreds node (Adjacency _ preds) = preds Map.! node

containsNode :: Ord node => node -> Adjacency node -> Bool
containsNode node (Adjacency succs _) = Map.member node succs

containsAdjacency :: Ord node => node -> node -> Adjacency node -> Bool
containsAdjacency src dst (Adjacency succs _) = 
	if Map.member src succs
	then elem dst (succs Map.! src)
	else False

-- UTILS
-------------------------------------------------------------------------------

dropElem :: Ord a => a -> [a] -> [a]
dropElem x xs = filter (\n -> n /= x) xs

