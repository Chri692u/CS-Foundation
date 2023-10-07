module Main where

import Graph

main :: IO ()
main = do
    let numVertices = 5  -- Change this to the number of vertices you want in your graph

    -- Create an empty graph with the specified number of vertices
    let emptyG = emptyGraph numVertices
    putStrLn "Empty Graph:"
    printGraph emptyG

    -- Add some edges to the graph
    let gWithEdges = foldl (\g (v1, v2) -> addEdge g v1 v2) emptyG [(1, 2), (2, 3), (3, 4), (4, 5)]
    putStrLn "Graph with Edges:"
    printGraph gWithEdges

    -- Check if there are edges between vertices
    putStrLn $ "Does edge (1, 2) exist? " ++ show (hasEdge gWithEdges 1 2)
    putStrLn $ "Does edge (2, 4) exist? " ++ show (hasEdge gWithEdges 2 4)

    -- Remove an edge
    let gWithoutEdge = removeEdge gWithEdges 1 2
    putStrLn "Graph after removing edge (1, 2):"
    printGraph gWithoutEdge

printGraph :: Graph -> IO ()
printGraph graph = putStrLn (show (adjacencyMatrix graph))
