module Main where

import Impure.STQueue

exampleQueue = do
    push 1
    push 2
    currentQueue <- peek
    pop

main :: IO ()
main = do
    let result = runQueue exampleQueue emptyQueue
    case result of
        Just (res, finalQueue) -> do
            putStrLn "Popped element:"
            print res
            putStrLn "Final Queue State:"
            print finalQueue
        Nothing -> putStrLn "Computation failed"