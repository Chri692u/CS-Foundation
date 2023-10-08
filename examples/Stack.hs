module Main where

import Impure.STStack
import Data.Maybe

examplePush = do
    push 1
    push 2
    push 3
    push 4

examplePop = do 
    pop
    pop

main :: IO ()
main = do
    let initialStack = emptyStack
    putStrLn "Pushing elements onto the stack:"
    let stack = snd $ fromJust $ runStack examplePush initialStack
    print stack

    putStrLn "Popping twice:"
    let stack' = snd $ fromJust $ runStack examplePop stack
    print stack'