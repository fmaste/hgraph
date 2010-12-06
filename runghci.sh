#!/bin/sh
cd `dirname "$0"`
ghci Data.Graph.hs Data.Graph.Digraph.hs Data.Graph.DAG.hs Graphviz.hs

