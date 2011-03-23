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

	---- CONSTRUCTION FUNCTIONS -------------------------------------------
	-----------------------------------------------------------------------

	-- Inserts a node to the graph without making any connection.
	addNode 	:: (Ord node, Ord edge) => node -> graph node edge -> graph node edge

	-- Deletes a node and all its connections from the graph.
	removeNode	:: (Ord node, Ord edge) => node -> graph node edge -> graph node edge

	-- Creates a link from the first node to the second.
	-- Is implementation dependent to decide if the order of the nodes is important (directed or undirected).
	-- Is implementation dependent to allow or not multiple edges (simple or multigraph).
	-- TODO: Add edge parameter
	addEdge		:: (Ord node, Ord edge) => node -> node -> graph node edge -> graph node edge

	-- Removes the link(s) from the first node to the second.
	-- Is implementation dependent to decide if the order of the nodes is important (directed or undirected).
	-- Is implementation dependent to allow or not multiple edges (simple or multigraph).
	-- The implementation may need extra methods to allow to delete a particular edge on a multigraph.
	-- TODO: Add edge parameter
	removeEdge	:: (Ord node, Ord edge) => node -> node -> graph node edge -> graph node edge

	-- TODO: After adding the edge parameters above, add this method
	-- removeEdges node node

	---- QUERY FUNCTIONS --------------------------------------------------
	-----------------------------------------------------------------------

	-- Gets a list of the nodes of the graph. Also called vertices or points.
	getNodes 		:: (Ord node, Ord edge) => graph node edge -> [node]

	-- Gets a lists with all the connection pairs. 
	-- Is implementation dependent to decide if the order of the nodes is important (directed or undirected).
	-- Is implementation dependent to allow or not multiple edges (simple or multigraph).
	getEdges 		:: (Ord node, Ord edge) => graph node edge -> [(node, node)]

	-- Gets the number of nodes.
	-- The implementation may override this method with a more performant one.
	getNodeCount	:: (Ord node, Ord edge) => graph node edge -> Int
	getNodeCount graph = length $ getNodes graph

	-- Gets the number of edges.
	-- The implementation may override this method with a more performant one.
	getEdgeCount	:: (Ord node, Ord edge) => graph node edge -> Int
	getEdgeCount graph = length $ getEdges graph

	-- True is the node exists, otherwise false.
	-- The implementation may override this method with a more performant one.
	containsNode	:: (Ord node, Ord edge) => node -> graph node edge -> Bool
	containsNode node graph = elem node (getNodes graph)

	-- True if the edge exists, otherwise false.
	-- The implementation may override this method with a more performant one.
        -- Is implementation dependent to decide if the order of the nodes is important (directed or undirected).
	containsEdge	:: (Ord node, Ord edge) => (node, node) -> graph node edge -> Bool
	containsEdge edge graph = elem edge (getEdges graph)

	-- Gets a list of all the nodes reachable from a given node.
	-- Is implementation dependent to allow or not multiple edges (simple or multigraph).
	reachable	:: (Ord node, Ord edge) => node -> graph node edge -> [node]

	-- Gets all the connections pairs that the node partipates.
	-- Is implementation dependent to decide if the order of the nodes is important (directed or undirected).
	-- Is implementation dependent to allow or not multiple edges (simple or multigraph).
	nodeEdges	:: (Ord node, Ord edge) => node -> graph node edge -> [(node, node)]

	-- TODO: Degree, indegree and outdegree.
	----------------------------------------

