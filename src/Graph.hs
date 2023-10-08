module Graph where

import Matrix
import Data.Array

-- | Data type representing an adjacency matrix for an undirected graph.
type AdjMatrix = Matrix Bool

-- | Data type representing an undirected graph.
data Graph = Graph
    { numVertices :: Int         -- ^ Number of vertices in the graph.
    , adjacencyMatrix :: AdjMatrix -- ^ Adjacency matrix of the graph.
    }

-- | Create an empty graph with a specified number of vertices.
emptyGraph :: Int -> Graph
emptyGraph n = Graph n (Matrix n n (array ((1, 1), (n, n)) [((i, j), False) | i <- [1..n], j <- [1..n]]))

-- | Add an edge between two vertices in the graph.
addEdge :: Graph -> Int -> Int -> Graph
addEdge graph v1 v2
    | v1 <= numVertices graph && v2 <= numVertices graph = Graph (numVertices graph) updatedMatrix
    | otherwise = error "Invalid vertex"
    where
        updatedMatrix = setEdge (adjacencyMatrix graph) v1 v2 True

-- | Remove an edge between two vertices in the graph.
removeEdge :: Graph -> Int -> Int -> Graph
removeEdge graph v1 v2
    | v1 <= numVertices graph && v2 <= numVertices graph = Graph (numVertices graph) updatedMatrix
    | otherwise = error "Invalid vertex"
    where
        updatedMatrix = setEdge (adjacencyMatrix graph) v1 v2 False

-- | Check if there is an edge between two vertices in the graph.
hasEdge :: Graph -> Int -> Int -> Bool
hasEdge graph v1 v2
    | v1 <= numVertices graph && v2 <= numVertices graph = getEdge (adjacencyMatrix graph) v1 v2
    | otherwise = False

-- Helper functions to manipulate the adjacency matrix
setEdge :: AdjMatrix -> Int -> Int -> Bool -> AdjMatrix
setEdge matrix v1 v2 val = matrix { elements = set (elements matrix) }
    where
        set arr = arr // [((v1, v2), val), ((v2, v1), val)]

getEdge :: AdjMatrix -> Int -> Int -> Bool
getEdge matrix v1 v2 = get (elements matrix)
    where
        get arr = arr ! (v1, v2)