module Main where

import Data.IORef
import TestMatrix

incr :: Num a => IORef a -> IO ()
incr c = modifyIORef' c (+1)

main :: IO ()
main = do
    propteries <- newIORef (0 :: Integer)
    putStrLn "Running property tests..."
    matrixTest <- runMatrixTest
    if matrixTest
        then do 
            incr propteries
            putStrLn "Matrix tests passed :D"
        else putStrLn "Some matrix tests failed :("

    passed <- readIORef propteries
    putStrLn $ "Total test files passed: " ++ show passed