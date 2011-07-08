-- Author: Federico Mastellone (fmaste@gmail.com)

-- Graph classes.
-- TODO: Make it haddock compatible!

-- MODULE
-------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Collection.Graph (
	Graph(..) ) where

-- IMPORTS
-------------------------------------------------------------------------------

import qualified Data.Collection as DC

-- CLASSES
-------------------------------------------------------------------------------

-- The main Graph class.
class (DC.Collection g, DC.Collection (Nodes g), DC.Collection (Edges g)) => Graph g where
	type Nodes g
	type Edges g

	-- ATOMIC CONSTRUCTION FUNCTIONS
	-----------------------------------------------------------------------

	-- Inserts a node to the graph without adding any Edge.
	addNode :: DC.Element (Nodes g) -> g -> g

	-- Deletes a node and all its Edges from the graph.
	removeNode :: DC.Element (Nodes g) -> g -> g

	-- Inserts an Edge to the graph adding Nodes if necessary.
	addEdge :: DC.Element (Edges g) -> g -> g

	-- Inserts a node to the graph without removing any Node.
	removeEdge :: DC.Element (Edges g) -> g -> g

	-- ATOMIC QUERY FUNCTIONS
	-----------------------------------------------------------------------

	-- The Nodes Collection.
	getNodes :: g -> Nodes g

	-- The Edges Collection.
	getEdges :: g -> Edges g

