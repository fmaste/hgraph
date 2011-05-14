-- Author: Federico Mastellone (fmaste@gmail.com)

-- Module that describes a node and its inmediate relationships with other nodes.
-- There are two types of inmediate relationsips that a node can have:
-- Being a successor or a predecessor to another node.
-- Higher grade relationships are not handled by this module.
-- Also, if node a and b are connected in the same direction by two different edges, 
-- each one with different values, this module only tells that they are connected 
-- by an edge, but not which one. You can't distinguish edges.
-- In short, this module handles two-way inmediate relationships between things of the same type.
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
	getNodeSuccNodes,
	getNodePredNodes,
	getNodeSuccNodesSet,
	getNodePredNodesSet,
	getNodeAdjacentNodes,
	getNodeAdjacentNodesSet,
	getNodeAdjacencies,
	getNodeSuccAdjacencies,
	getNodePredAdjacencies,
	getAdjacencies,
	getAdjacencyCount,
	containsNode,
	containsNodeSucc,
	containsNodePred,
	containsAdjacency,
	revert) where

-- IMPORTS
-------------------------------------------------------------------------------

import Data.List (foldl, foldl', foldr)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.BinaryRelation as BR

-- * DATA DEFINITION
-------------------------------------------------------------------------------

-- | The adjacency of a graph here is defined with an incidence list with the 
-- direct node successors and another one with the direct predecessors.
-- This incidence lists only represent to which nodes a node is connected by 
-- a single edge. Being the difference between both lists the diretion of the 
-- edge, either "to" or "from" the node.
data Adjacency node = Adjacency (BR.BinaryRelation node node)
    deriving (Show, Read, Ord, Eq)

-- * CONSTRUCTION FUNCTIONS
-------------------------------------------------------------------------------

-- | The empty Adjacency.
empty :: Ord node => Adjacency node
empty = Adjacency BR.empty

-- | Adds a node without any adjacencies.
-- If the node already exists the original Adjacency is returned.
addNode :: Ord node => node -> Adjacency node -> Adjacency node
addNode node (Adjacency br) = Adjacency br' where
	br' = BR.addCodomainElement node $ BR.addDomainElement node br

-- | Removes the node and all the adjacencies were the node participates.
-- If the node does not exists the original Adjacency is returned.
removeNode :: Ord node => node -> Adjacency node -> Adjacency node
removeNode node (Adjacency br) = Adjacency br' where
	br' = BR.removeCodomainElement node $ BR.removeDomainElement node br

-- | Adds an adjacency from src to dst.
-- If src or dst do not exist they are added.
-- If the adjacency already exists the original Adjacency is returned.
addAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
addAdjacency src dst (Adjacency br) = Adjacency br' where
	br' = BR.addRelation src dst br

-- | Removes the adjacency from src to dst.
-- If src or dst do not exist the original Adjacency is returned.
-- If the adjancecy does not exists the original Adjacency is returned.
removeAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
removeAdjacency src dst (Adjacency br) = Adjacency br' where
	br' = BR.removeRelation src dst br

-- | Removes all the adjacencies between node1 and node2.
-- If node1 or node2 do not exist the original Adjacency is returned.
-- If no adjacencies between node1 and node2 exist the original Adjacency is returned.
removeFullAdjacency :: Ord node => node -> node -> Adjacency node -> Adjacency node
removeFullAdjacency node1 node2 adj = 
	removeAdjacency node1 node2 $ removeAdjacency node2 node1 adj

-- | Removes all the adjacencies were this node participates.
-- If node does not exists the original Adjacency is returned.
-- If there are no adjacencies were this node participates the original Adjacency is returned.
removeNodeAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodeAdjacencies node adj =
	removeNodeSuccAdjacencies node $ removeNodePredAdjacencies node adj

-- | Removes all the adjacencies were this node is predecessor.
-- If node does not exists the original Adjacency is returned.
-- If there are no adjacencies were this node is predecessor the original Adjacency is returned.
removeNodeSuccAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodeSuccAdjacencies node adj@(Adjacency br) = Adjacency br' where
	br' = foldl' removeSuccFromPreds br (getNodeSuccNodes node adj) where
		removeSuccFromPreds br'' succNode = BR.removeRelation node succNode br''

-- | Removes all the adjacencies were this node is successor.
-- If node does not exists the original Adjacency is returned.
-- If there are no adjacencies were this node is successor the original Adjacency is returned.
removeNodePredAdjacencies :: Ord node => node -> Adjacency node -> Adjacency node
removeNodePredAdjacencies node adj@(Adjacency br) = Adjacency br' where
	br' = foldl' removePredFromSuccs br (getNodePredNodes node adj) where
		removePredFromSuccs br'' predNode = BR.removeRelation predNode node br''

-- * QUERY FUNCTIONS
-------------------------------------------------------------------------------

-- | A list with all the different nodes.
getNodes :: Ord node => Adjacency node -> [node]
getNodes (Adjacency br) = BR.getDomainElements br

-- | The number of different nodes present.
getNodeCount :: Ord node => Adjacency node -> Int
getNodeCount (Adjacency br) = BR.getDomainCount br

-- | Get all the different nodes that are successors.
getNodeSuccNodes :: Ord node => node -> Adjacency node -> [node]
getNodeSuccNodes node (Adjacency br) = BR.getRelatedToElements node br

-- | Get all the different nodes that are predecessors.
getNodePredNodes :: Ord node => node -> Adjacency node -> [node]
getNodePredNodes node (Adjacency br) = BR.getRelatedFromElements node br

-- | A set with the node successors.
getNodeSuccNodesSet :: Ord node => node -> Adjacency node -> Set.Set node
getNodeSuccNodesSet node (Adjacency br) = BR.getRelatedTo node br

-- | A set with the node predecessors.
getNodePredNodesSet :: Ord node => node -> Adjacency node -> Set.Set node
getNodePredNodesSet node (Adjacency br) = BR.getRelatedFrom node br

-- | The different nodes that are adjacencent, either succs or preds.
getNodeAdjacentNodes :: Ord node => node -> Adjacency node -> [node]
getNodeAdjacentNodes node adj = 
	Set.elems $ getNodeAdjacentNodesSet node adj

-- | A set with the different nodes that are adjacencent, either succs or preds.
getNodeAdjacentNodesSet :: Ord node => node -> Adjacency node -> Set.Set node
getNodeAdjacentNodesSet node adj = 
	Set.union (getNodeSuccNodesSet node adj) (getNodePredNodesSet node adj)

-- | The different adjacencies were this node participates.
getNodeAdjacencies :: Ord node => node -> Adjacency node -> [(node, node)]
getNodeAdjacencies node adj = 
	getNodeSuccAdjacencies node adj ++ getNodePredAdjacencies node adj

-- | The different adjacencies were this node is predecessor.
getNodeSuccAdjacencies :: Ord node => node -> Adjacency node -> [(node, node)]
getNodeSuccAdjacencies node adj = [(node, x) | x <- getNodeSuccNodes node adj]

-- | The different adjacencies were this node is successor.
getNodePredAdjacencies :: Ord node => node -> Adjacency node -> [(node, node)]
getNodePredAdjacencies node adj = [(x, node) | x <- getNodePredNodes node adj]

-- | All the different adjacencies that exist.
getAdjacencies :: Ord node => Adjacency node -> [(node, node)]
getAdjacencies adj = 
	concatMap (\node -> [(node, x) | x <- getNodeSuccNodes node adj]) (getNodes adj)

-- | The number of different adjacencies.
getAdjacencyCount :: Ord node => Adjacency node -> Int
getAdjacencyCount (Adjacency br) = 
	foldl (\ans key -> ans + (BR.getRelatedToCount key br)) 0 $ BR.getDomainElements br

-- | Node exists?
containsNode :: Ord node => node -> Adjacency node -> Bool
containsNode node (Adjacency br) = BR.containsDomainElement node br

-- | Is this a successor of node?
containsNodeSucc :: Ord node => node -> node -> Adjacency node -> Bool
containsNodeSucc node succ adj = containsAdjacency node succ adj

-- | Is this a predecessor of node?
containsNodePred :: Ord node => node -> node -> Adjacency node -> Bool
containsNodePred node pred adj = containsAdjacency pred node adj

-- | Adjacency exists?
containsAdjacency :: Ord node => node -> node -> Adjacency node -> Bool
containsAdjacency src dst (Adjacency br) = BR.isRelatedTo src dst br

-- * CONVERSION
-------------------------------------------------------------------------------

-- | All connections are reverted, every src -> dst becomes src <- dst.
revert :: Ord node => Adjacency node -> Adjacency node
revert (Adjacency br) = Adjacency (BR.revert br) where

