-- Author: Federico Mastellone (fmaste@gmail.com)

module Graphviz (
	graphvizDigraph,
	command) where 

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

graphviz :: (Graph.Graph graph, Ord node, Ord edge) => (node -> String) -> String -> graph node edge -> String
graphviz toString edgeGlue graph = "digraph {" ++ body ++ "}" where
	body = implode ";" (nodes ++ edges) where
		nodes = map toString (Graph.getNodes graph)
		edges = map arc (Graph.getEdges graph) where
			arc (src,dst) = (toString src) ++ edgeGlue ++ (toString dst)
			stringId a = "\"" ++ (scape (toString a)) ++ "\""

graphvizGraph 	:: (Graph.Graph graph, Ord node, Ord edge) => (node -> String) -> graph node edge -> String
graphvizGraph 	toString graph		= graphviz toString "--" graph

graphvizDigraph	:: (Graph.Graph graph, Ord node, Ord edge) => (node -> String) -> graph node edge -> String
graphvizDigraph	toString digraph 	= graphviz toString "->" digraph


-- writeFile "graphviz.dot" (graphvizdigraph show digraph)
-- > cat graphviz.dot | dot -Tpng > graphviz.png

--	graphvizProperty (n,t) = "{" ++ (show n) ++ "|" ++ (show t) ++ "}"

