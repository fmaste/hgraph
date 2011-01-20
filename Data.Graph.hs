module Data.Graph where

-- The topmost module of the graph library.
-- A graph is an ordered pair G = (V, E) comprising a set V of nodes (or vertices) together with a set or multiset (bag) E of edges (or lines).
-- Any number of nodes is allowed as long as they are not repeated ones.
-- If E is a set, no repeated edges are allowed, and the graph is called simple. If E is a multiset, repeated edges are allowed and is called multigraph.
-- The elements of E are pairs of vertices, 
-- if the pair is ordered the lines have an orientation and the graph is called directed or digraph for short,
-- otherwise, the pair is unordered and there is no orientation, just a relation between two nodes and the graph is called undirected or simply a graph.
-- This differences and mucho more, like allowing cycles or an edge pair of the same node, are implementation depedent.
class Graph graph where

	---- CONSTRUCTOR FUNCTIONS --------------------------------------------
	-----------------------------------------------------------------------

	-- Inserts a node to the graph without making any connection.
	addNode 	:: (Ord node, Ord edge) => node -> graph node edge -> graph node edge

	-- Deletes a node and all its connections from the graph.
	delete 		:: (Ord node, Ord edge) => node -> graph node edge -> graph node edge

	-- Creates a link from the first node to the second.
	-- Is implementation dependent to decide if the order of the nodes is important (directed or undirected).
	-- Is implementation dependent to allow or not repeated edges (simple or multigraph).
	link 		:: (Ord node, Ord edge) => node -> node -> graph node edge -> graph node edge

	-- Removes the link(s) from the first node to the second.
	-- Is implementation dependent to decide if the order of the nodes is important (directed or undirected).
	-- Is implementation dependent to allow or not repeated edges (simple or multigraph).
	-- The implementation may need extra methods to allow to delete a particular edge on a multigraph.
	unlink 		:: (Ord node, Ord edge) => node -> node -> graph node edge -> graph node edge

	---- GETTER FUNCTIONS -------------------------------------------------
	-----------------------------------------------------------------------

	-- Gets a list of the nodes of the graph. Also called vertices or points.
	nodes 		:: (Ord node, Ord edge) => graph node edge -> [node]

	-- Gets a lists with all the connection pairs. 
	-- Is implementation dependent to decide if the order of the nodes is important (directed or undirected).
	-- Is implementation dependent to allow or not repeated edges (simple or multigraph).
	edges 		:: (Ord node, Ord edge) => graph node edge -> [(node, node)]

	-- Gets a list of all the nodes reachable from a given node.
	-- Is implementation dependent to allow or not repeated edges (simple or multigraph).
	reachable	:: (Ord node, Ord edge) => node -> graph node edge -> [node]

	-- Gets all the connections pairs that the node partipates.
	-- Is implementation dependent to decide if the order of the nodes is important (directed or undirected).
	-- Is implementation dependent to allow or not repeated edges (simple or multigraph).
	nodeEdges	:: (Ord node, Ord edge) => node -> graph node edge -> [(node, node)]

	-- True is the node exists, otherwise false.
	-- The implementation may override this method with a more performant one.
	nodeExists	:: (Ord node, Ord edge) => node -> graph node edge -> Bool
	nodeExists node graph = elem node (nodes graph)

	-- True if the edge exists, otherwise false.
	-- The implementation may override this method with a more performant one.
        -- Is implementation dependent to decide if the order of the nodes is important (directed or undirected).
	edgeExists	:: (Ord node, Ord edge) => (node, node) -> graph node edge -> Bool
	edgeExists edge graph = elem edge (edges graph)

	-- TODO: Degree, indegree and outdegree.
	----------------------------------------

