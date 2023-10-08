module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Lattice

-- Babylon's number is approximated using f x = (0.5 * (x + 2 / x)).
babylonsNumber :: Double -> Double
babylonsNumber x = 0.5 * (x + 2 / x)

main :: IO ()
main = do
  let elements = Set.fromList [1.0, 1.001 .. 2.0] :: Set Double

  -- Check if the lattice is complete and print the result.
  let isCompleteLattice = isComplete elements
  putStrLn $ "Is the lattice complete? " ++ show isCompleteLattice

  -- Use Tarski's fixed-point theorem to approximate Babylon's number within the lattice.
  let fixedPoint = tarski babylonsNumber 1.0 elements
  putStrLn $ "Approximated Babylon's number within the lattice: " ++ show fixedPoint