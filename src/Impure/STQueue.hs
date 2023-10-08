module Impure.STQueue where

import Control.Monad.Trans.State
import Control.Monad.Cont

data Queue a = Queue [a] [a] 
    deriving (Show)

emptyQueue :: Queue a
emptyQueue = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue front rear) = Queue front (x : rear)

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue (x : xs) rear) = Just (x, Queue xs rear)
dequeue (Queue [] rear) = dequeue (Queue (reverse rear) [])

-- Check if the queue is empty
isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

type STQueue a = StateT (Queue a) Maybe

-- Push an element onto the queue
push :: a -> STQueue a ()
push x = modify (enqueue x)

-- Pop an element from the queue
pop :: STQueue a a
pop = do
    q <- get
    case dequeue q of
        Just (x, q') -> do
            put q'
            return x
        Nothing -> lift Nothing

peek :: STQueue a (Queue a)
peek = get

runQueue :: STQueue a b -> Queue a -> Maybe (b, Queue a)
runQueue = runStateT