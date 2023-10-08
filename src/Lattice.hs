{-# LANGUAGE InstanceSigs #-}
module Lattice where

import Data.Set (Set)
import qualified Data.Set as Set

-------------------
-- DATASTRUCTURE --
-------------------

-- | A type class Lattice, which represents a lattice structure.
class (Ord a, Eq a) => Lattice a where
  rel :: a -> a -> Bool  -- Describes the relation between elements of the lattice
  glb :: a -> a -> a    -- Computes the Greatest Lower Bound (glb) of two lattice elements
  lub :: a -> a -> a    -- Computes the Least Upper Bound (lub) of two lattice elements

instance Lattice Double where
  rel :: Double -> Double -> Bool
  rel x y = x <= y

  glb :: Double -> Double -> Double
  glb = min

  lub :: Double -> Double -> Double
  lub = max

instance Lattice Bool where
  rel :: Bool -> Bool -> Bool
  rel x y = x <= y

  glb :: Bool -> Bool -> Bool
  glb = (&&)

  lub :: Bool -> Bool -> Bool
  lub = (||)

instance Lattice Int where
  rel :: Int -> Int -> Bool
  rel x y = x <= y

  glb :: Int -> Int -> Int
  glb = min
  
  lub :: Int -> Int -> Int
  lub = max

-- In a complete lattice, every subset has both a glb and a lub.
-- This function checks whether all possible pairs of elements satisfy this property.
isComplete :: (Lattice a) => Set a -> Bool
isComplete elements =
  let
    pairs = [(x, y) | x <- Set.toList elements, y <- Set.toList elements]
  in
    all (\(x, y) -> glb x y `Set.member` elements && lub x y `Set.member` elements) pairs

-------------------
-- Algorithms    --
-------------------

-- | Tarski's fixed-point theorem
tarski :: (Lattice a, Ord a) => (a -> a) -> a -> Set a -> a
tarski f first lattice
  | not (isComplete lattice) = error "The lattice is not complete." -- Check lattice completeness
  | otherwise = fixpoint f first lattice
  where
    fixpoint g x lat
      | lat' == lat = x  -- If the lattice remains unchanged, we've found a fixed point
      | otherwise = fixpoint g x' lat'
      where
        step = lub x (g x)          -- Calculate the next candidate using lub
        lat' = Set.insert step lat  -- Update the lattice with the new candidate
        x' = lub x (g step)       -- Calculate the new candidate's lub with its image
