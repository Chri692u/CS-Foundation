{-# LANGUAGE TemplateHaskell #-}

module TestMatrix where

import Test.QuickCheck
import Matrix

instance Arbitrary a => Arbitrary (Matrix a) where
    arbitrary = do
        size <- choose (1, 10)  -- You can adjust the range of the square matrix size.
        es <- vectorOf (size * size) arbitrary
        return $ reshape size size es


-- Property: Transposing twice yields the original matrix.
prop_transposeTransposeIsOriginal :: Matrix Int -> Property
prop_transposeTransposeIsOriginal m =
    let m' = transpose m
    in transpose m' === m

-- Property: The number of rows and columns in a transposed matrix are swapped.
prop_transposeShape :: Matrix Int -> Bool
prop_transposeShape m =
    let (rows', cols') = shape m
        (rowsT, colsT) = shape (transpose m)
    in rows' == colsT && cols' == rowsT

-- Property: Matrix addition is commutative.
prop_matrixAddCommutative :: Matrix Int -> Matrix Int -> Bool
prop_matrixAddCommutative m1 m2 = m1 + m2 == m2 + m1

-- Property: Matrix multiplication is associative.
prop_matrixMultAssociative :: Matrix Int -> Matrix Int -> Matrix Int -> Bool
prop_matrixMultAssociative m1 m2 m3 = m1 * (m2 * m3) == (m1 * m2) * m3

runMatrixTest :: IO Bool
runMatrixTest = $quickCheckAll