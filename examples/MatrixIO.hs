module Main where

import Matrix

main :: IO ()
main = do
    let zeros = reshape 2 2 [0]
    print zeros
    let ones = reshape 2 2 [1]
    print ones
    let mixed = reshape 2 3 [-1,0,3,4.0]
    let id = reshape 2 2 [1, 0, 0, 1]
    print id
    let res1 = shape id
    let res2 = id + id
    
    -- Multiplication
    let res3 = id * ones

    -- Scalar to matrix
    let five = 5 :: Matrix Int

    -- Negation
    let negM = negate ones

    -- Absolute values of each entry
    let absM = abs mixed

    -- Signum of each entry
    let signM = signum mixed

    -- Transpose
    let mixedT = transpose mixed
    print mixed