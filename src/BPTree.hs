{-# LANGUAGE InstanceSigs #-}

module BPTree where

import Control.Monad
import Data.Maybe
import Data.Binary
import Control.Arrow

-- Type synonyms
type ValuePair k v = (k, v)
type PointerPair k v = (k, BPTree k v)

-- B+ Tree type
data BPTree k v = Leaf [ValuePair k v] | Node [PointerPair k v]

instance Functor (BPTree k) where
  fmap :: (a -> b) -> BPTree k a -> BPTree k b
  fmap f (Leaf kvs) = Leaf $ fmap (second f) kvs
  fmap f (Node kts) = Node $ fmap (second (fmap f)) kts

-- Show instance for B+ tree
instance (Show k, Show v) => Show (BPTree k v) where
  show :: (Show k, Show v) => BPTree k v -> String
  show tree = show' tree ""
    where show' (Leaf kvs) indent = indent ++ "Leaf:\n" ++ unlines (map (showKeyValuePair indent) kvs)
          show' (Node kts) indent = indent ++ "Node:\n" ++ unlines (map (showKeyPointerPair indent) kts)
          showKeyValuePair indent (k, v) = indent ++ "Key: " ++ show k ++ ", Value: " ++ show v
          showKeyPointerPair indent (k, t) = indent ++ "Key: " ++ show k ++ "\n" ++ show' t (indent ++ "  ")

-- Binary instance for B+ tree
instance (Binary k, Binary v) => Binary (BPTree k v) where
  put :: (Binary k, Binary v) => BPTree k v -> Put
  put (Leaf kvs) = putWord8 0 >> put kvs
  put (Node kts) = putWord8 1 >> put kts

  get :: (Binary k, Binary v) => Get (BPTree k v)
  get = do
    tag <- getWord8
    case tag of
      0 -> Leaf <$> get
      1 -> Node <$> get
      _ -> fail "Invalid tag"

-- Insert a key-value pair into the B+ tree
insert :: Ord k => k -> v -> BPTree k v -> BPTree k v
insert key value (Leaf kvs) = Leaf (insertInOrder key value kvs)
insert key value (Node kts) = case insertPointer key value kts of
    [] -> error "Invalid state: insertPointer returned an empty list"
    [(newKey, newTree)] -> Node [(newKey, newTree)]  -- Create a new root node if necessary
    newKts -> Node newKts

-- Helper function to insert a key-value pair into a sorted list
insertInOrder :: Ord k => k -> v -> [ValuePair k v] -> [ValuePair k v]
insertInOrder key value kvs = do
    (key', value') <- kvs
    guard (key >= key')
    if key == key'
        then return (key, value)
    else return (key', value')

-- Helper function to insert a pointer into the list of pointers
insertPointer :: Ord k => k -> v -> [PointerPair k v] -> [PointerPair k v]
insertPointer key value kts = do
    (k, t) <- kts
    guard (key >= k)
    return $ if key == k
        then (k, insert key value t)
    else (k, t)
    ++ [(key, Leaf [(key, value)])]

-- Helper function to insert a new key-value pair into the list of pointers
insertNewKey :: Ord k => k -> v -> [PointerPair k v] -> [PointerPair k v]
insertNewKey key value kts = do
  (k, t) <- kts
  guard (key >= k)
  return $ if key == k
    then (k, Leaf ((key, value) : getPairs t))
    else (k, t)
  ++ [(key, Leaf [(key, value)])]
  where
    getPairs (Leaf pairs) = pairs
    getPairs (Node _) = error "Invalid state: cannot retrieve pairs from a non-leaf node"