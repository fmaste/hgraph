module Graphviz (
	graphvizDigraph,
	command) where 

--import qualified Data.Class
import qualified Data.Graph as Graph
import qualified Data.Graph.Digraph as Digraph

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

graphviz :: (Graph.Graph graph, Ord node) => (node -> String) -> String -> graph node -> String
graphviz toString edgeGlue graph = "digraph {" ++ body ++ "}" where
	body = implode ";" (nodes ++ edges) where
		nodes = map toString (Graph.nodes graph)
		edges = map arc (Graph.edges graph) where
--		edges = map arc [(src,dst) | src <- (Graph.nodes graph), dst <- (Graph.heads src graph)] where
			arc (src,dst) = (toString src) ++ edgeGlue ++ (toString dst)
			stringId a = "\"" ++ (scape (toString a)) ++ "\""

graphvizGraph 	:: (Graph.Graph graph, Ord node) => (node -> String) -> graph node -> String
graphvizGraph 	toString graph		= graphviz toString "->" graph

graphvizDigraph	:: (Graph.Graph graph, Ord node) => (node -> String) -> graph node -> String
graphvizDigraph	toString digraph 	= graphviz toString "->" digraph


-- writeFile "graphviz.dot" (graphvizdigraph show digraph)
-- > cat graphviz.dot | dot -Tpng > graphviz.png

--classNode :: Data.Class.Data.Class -> String
--classNode c = (name c) ++ "[label='{" ++ (implode "|" (map graphvizProperty (properties c))) ++ "}']" where

--	graphvizProperty (n,t) = "{" ++ (show n) ++ "|" ++ (show t) ++ "}"

