module Main where

import BPTree
import Algorithms

main :: IO ()
main = do
    -- Initial B+ tree
    let leaf1 = Leaf [("apple", 1), ("apricot", 2)]
    let leaf2 = Leaf [("banana", 3), ("blueberry", 4)]
    let leaf3 = Leaf [("cherry", 5), ("coconut", 6)]
    let tree = Node
            [ ("apple", leaf1),
              ("banana", Node [("blackberry", leaf2), ("cherry", leaf3)]),
              ("grape", Leaf [("grape", 7)])
            ]

    -- Insert new key-value pairs
    let treeWithInsertions = insert "kiwi" 8 tree
    let treeWithInsertions2 = insert "lemon" 9 treeWithInsertions

    -- Search for values
    putStrLn "Searching for values:"
    putStrLn $ "apple: " ++ show (search "apple" treeWithInsertions2)
    putStrLn $ "banana: " ++ show (search "banana" treeWithInsertions2)
    putStrLn $ "kiwi: " ++ show (search "kiwi" treeWithInsertions2)
    putStrLn $ "lemon: " ++ show (search "lemon" treeWithInsertions2)
    putStrLn $ "orange: " ++ show (search "orange" treeWithInsertions2)

    print treeWithInsertions2