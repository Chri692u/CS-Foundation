{-# LANGUAGE InstanceSigs #-}

module Matrix where

import Data.Array
import Data.Array.Base(amap)
import Data.List

-- | Type synonym for a row in a matrix.
type Row a = (Array Int a)

-- | Data type representing a matrix.
data Matrix a = Matrix
    { rows :: Int              -- ^ Number of rows in the matrix.
    , cols :: Int              -- ^ Number of columns in the matrix.
    , elements :: Array (Int, Int) a  -- ^ Array storing the elements of the matrix.
    }

instance (Show a) => Show (Matrix a) where
    -- | Convert a matrix to its string representation.
    show :: (Show a) => Matrix a -> String
    show matrix =
        let m = elements matrix
            ((r1, c1), (rn, cn)) = bounds m
        in unlines
            [ intercalate "   " [show $ m ! (r, c) | c <- [c1 .. cn]]
            | r <- [r1 .. rn]
            ]

instance Functor Matrix where
    -- | Map a function over the elements of the matrix.
    fmap :: (a -> b) -> Matrix a -> Matrix b
    fmap f matrix = matrix { elements = fmap f (elements matrix) }

instance Eq a => Eq (Matrix a) where
    -- | Check if two matrices are equal.
    (==) :: Eq a => Matrix a -> Matrix a -> Bool
    m1 == m2 = shapesOk && valuesOk
        where   shapesOk = shape m1 == shape m2
                valuesOk = and $ zipWith (==) l1 l2
                l1 = toList m1
                l2 = toList m2

instance Num a => Num (Matrix a) where
    -- | Add two matrices element-wise.
    (+) :: Num a => Matrix a -> Matrix a -> Matrix a
    m1 + m2
        | shape m1 /= shape m2 = error "DIMENSION ERROR" zipWith (+) l1 l2
        | otherwise = reshape r c $ zipWith (+) l1 l2
            where r = rows m1
                  c = cols m1
                  l1 = toList m1
                  l2 = toList m2

    -- | Multiply two matrices using the standard matrix multiplication algorithm.
    (*) :: Num a => Matrix a -> Matrix a -> Matrix a
    m1 * m2 = innerProduct (+) (*) m1 m2

    -- | Convert an integer to a 1x1 matrix.
    fromInteger :: Num a => Integer -> Matrix a
    fromInteger n = Matrix 1 1 (array (ones, ones) [(ones, fromInteger n)])
        where ones = (1,1)

    -- | Negate all elements of the matrix.
    negate :: Num a => Matrix a -> Matrix a
    negate = fmap negate

    -- | Apply the absolute value function to all elements of the matrix.
    abs :: Num a => Matrix a -> Matrix a
    abs = fmap abs

    -- | Apply the 'signum' function to all elements of the matrix.
    signum :: Num a => Matrix a -> Matrix a
    signum = fmap signum

-- | Lookup the value at a specific row and column.
get :: Matrix a -> Int -> Int -> a
get matrix row col = elements matrix ! (row, col)

-- | Convert a matrix to an array of rows.
asRows :: Matrix a -> Array Int (Row a)
asRows m = array (1, ncols) [(i, col i) | i <- [1..ncols]]
    where m' = elements m
          ncols = cols m
          nrows = rows m
          col i = array (1, nrows) [(j, m' ! (j, i)) | j <- [1..nrows]]

-- | Convert a matrix to a list.
toList :: Matrix a -> [a]
toList matrix = [get matrix r c | r <- [1..rows matrix], c <- [1..cols matrix]]

-- | Return the shape of the matrix (number of rows and columns).
shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

-- | Return a matrix of the specified shape with elements from the provided list (wrapping if necessary).
reshape :: Int -> Int -> [a] -> Matrix a
reshape rows cols vs =
    let n = rows * cols
        values = take n (cycle vs)
        indexed = zipWith (\i v -> ((i `div` cols + 1, i `mod` cols + 1), v)) [0..] values
        bounds = ((1, 1), (rows, cols))
        arr = array bounds indexed
    in Matrix rows cols arr

-- | Perform the inner product of two matrices using provided addition and multiplication functions.
innerProduct :: Num a => (a -> a -> a) -> (b -> c -> a) -> Matrix b -> Matrix c -> Matrix a
innerProduct f g m1 m2
    | cols m1 /= rows m2 = error "SIZE ERROR: Incompatible sizes for inner product"
    | otherwise = reshape (rows m1) (cols m2) [ getList r c | r <- [1..rows m1], c <- [1..cols m2]]
    where
        getList r c = foldl1 f (zipWith g (getRow m1 r) (getCol m2 c))
        getRow m r = [get m r c | c <- [1..cols m]]
        getCol m c = [get m r c | r <- [1..rows m]]

-- | Transpose a matrix (rows become columns, and vice versa).
transpose :: Matrix a -> Matrix a
transpose matrix = reshape (cols matrix) (rows matrix) $ transpose' matrix
  where
    transpose' m = [get m c r | r <- [1..cols m], c <- [1..rows m]]

upperTriangular :: Num a => Matrix a -> Matrix a
upperTriangular matrix = matrix { elements = array bounds updatedElements }
  where
    bounds = ((1, 1), (rows matrix, cols matrix))
    updatedElements = [((r, c), if r < c then 0 else get matrix r c) | (r, c) <- range bounds]
    get m r c = elements m ! (r, c)

-- | Convert a matrix into an Array
flatten :: Matrix a -> Array Int a
flatten matrix
    | cols matrix == 1 = array (1, rows matrix) $ zip [1..] (map (\i -> elements matrix ! (i, 1)) [1..rows matrix])
    | rows matrix == 1 = array (1, cols matrix) $ zip [1..] (map (\i -> elements matrix ! (1, i)) [1..cols matrix])
    | otherwise = error "VECTOR LENGTH ERROR"