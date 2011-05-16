-- Author: Federico Mastellone (fmaste@gmail.com)

module Graphviz (
	graphvizDigraph,
	command) where 

import qualified Data.Set as Set
import qualified Data.BinaryRelation as BR
import qualified Data.Graph as Graph

implode :: String -> [String] -> String
implode glue []     = ""
implode glue (x:[]) = x
implode glue (x:xs) = x ++ glue ++ (implode glue xs)

scape :: String -> String
scape [] = []
scape (x:xs) = (replace x) ++ (scape xs) where
	replace x
		| x == '"' 	= "\\\""
		| otherwise 	= [x]

command :: String -> String
command dot = "echo '" ++ dot ++ "' | dot -Tpng > dot.png"

graphviz :: [String] -> [(String, String)] -> String -> String
graphviz nodes edges edgeGlue = "digraph {" ++ body ++ "}" where
	body = implode ";" (nodes ++ edgesString) where
		edgesString = map arc edges where
			arc (src,dst) = src ++ edgeGlue ++ dst

graphvizBinaryRelation :: (Ord domain, Ord codomain) => (domain -> String) -> (codomain -> String) -> BR.BinaryRelation domain codomain -> String
graphvizBinaryRelation domainToString codomainToString br = graphviz nodesString edgesString "-->" where
	nodesString = (map domainToString $ BR.getDomainElements br) ++ (map codomainToString $ BR.getCodomainElements br)
	edgesString = map arcString $ BR.getGraph br where
		arcString (src, dst) = (domainToString src, codomainToString dst)

graphvizGraph :: (Graph.Graph graph, Ord node, Ord edge) => (node -> String) -> graph node edge -> String
graphvizGraph toString graph = graphvizGraph' toString "--" graph

graphvizDigraph	:: (Graph.Graph graph, Ord node, Ord edge) => (node -> String) -> graph node edge -> String
graphvizDigraph	toString digraph = graphvizGraph' toString "->" digraph

graphvizGraph' :: (Graph.Graph graph, Ord node, Ord edge) => (node -> String) -> String -> graph node edge -> String
graphvizGraph' toString edgeGlue graph = graphviz nodes edges edgeGlue where
	nodes = map toString $ Graph.getNodes graph
	edges = map arcString $ Graph.getEdges graph where
		arcString (src,dst) = (toString src, toString dst)

-- writeFile "graphviz.dot" (graphvizdigraph show digraph)
-- > cat graphviz.dot | dot -Tpng > graphviz.png

--	graphvizProperty (n,t) = "{" ++ (show n) ++ "|" ++ (show t) ++ "}"

