module Algorithms where

import Matrix
import Data.Array

-- | Solve the equation system by backward substitution.
solve :: (Fractional a, Eq a) => Matrix a -> Array Int a -> Array Int a
solve m = backwards (asRows m)

backwards :: (Eq a, Fractional a) => Array Int (Row a) -> Row a -> Array Int a
backwards m b = sol
  where
    n = snd $ bounds b
    sol = array (1, n) solution
    solution = [(check i m, (b ! i - sum [sol ! j * (m ! i ! j) | j <- [i + 1..n]]) / m ! i ! i) | i <- [n, n - 1..1]]

check :: (Eq a, Num a) => Int -> Array Int (Row a) -> Int
check i m = if m ! i ! i == 0 then error "Unsolvable equation system: Zero pivot element" else i

-- | QR decomposition of a matrix into two matrices Q and R.
decompQR :: (Eq a, Floating a, Ord a) => Matrix a -> (Matrix a, Matrix a)
decompQR m = (matQ, matR)
  where
    getCol m c = listArray (1, rows m) [get m r c | r <- [1..rows m]]
    columns = [getCol m row | row <- [1..cols m]]
    matQ = orthogonalize m
    matR = upperTriangular $ transpose $ matQ * m

-- | Compute the orthogonalized matrix using the Gram-Schmidt process.
orthogonalize :: (Eq a, Floating a) => Matrix a -> Matrix a
orthogonalize m = reshape (rows m) (cols m) (concatMap elems qs)
    where getCol m c = listArray (1, rows m) [get m r c | r <- [1..rows m]]
          columns = [getCol m row | row <- [1..cols m]]
          qs = gramSchmidt columns []

gramSchmidt :: (Fractional a, Eq a, Floating a) => [Array Int a] -> [Array Int a] -> [Array Int a]
gramSchmidt [] processed = processed
gramSchmidt (vec:vs) processed = gramSchmidt vs (processed ++ [step])
  where
    step = normalize $ process vec processed
    normalize vector = listArray (bounds vector) $ map (/ norm) (elems vector)
      where norm = sqrt (vector `dot` vector)

process :: (Fractional a, Eq a) => Array Int a -> [Array Int a] -> Array Int a
process vs = foldl (\acc q -> acc -. projection vs q) vs

projection :: (Eq a, Fractional a) => Array Int a -> Array Int a -> Array Int a
projection vec onto
    | normSq == 0 = error "Cannot project onto a zero vector."
    | otherwise = listArray (bounds vec) $ map (* scaleFactor) (elems onto)
    where
      dp = vec `dot` onto
      scaleFactor = dp / normSq
      normSq = onto `dot` onto

binop :: (a -> a -> a) -> Array Int a -> Array Int a -> Array Int a
binop f v1 v2
    | bounds v1 /= bounds v2 = error "VECTOR LENGTH ERROR"
    | otherwise = array (bounds v1) [(i, f (v1 ! i) (v2 ! i)) | i <- indices v1]

dot :: Num a => Array Int a -> Array Int a -> a
dot v1 v2
    | bounds v1 /= bounds v2 = error "VECTOR LENGTH ERROR"
    | otherwise = sum [v1 ! i * v2 ! i | i <- indices v1]

(-.) :: Num a => Array Int a -> Array Int a -> Array Int a
(-.) = binop (-)