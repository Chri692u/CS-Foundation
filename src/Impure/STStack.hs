module Impure.STStack where

import Control.Monad.Trans.State
import Control.Monad.Cont

newtype Stack a = Stack [a]
    deriving (Show)

emptyStack :: Stack a
emptyStack = Stack []

purePush :: a -> Stack a -> Stack a
purePush x (Stack xs) = Stack (x : xs)

purePop :: Stack a -> Maybe (a, Stack a)
purePop (Stack []) = Nothing
purePop (Stack (x:xs)) = Just (x, Stack xs)

type STStack a = StateT (Stack a) Maybe

push :: a -> STStack a ()
push x = modify (purePush x)

pop :: STStack a a
pop = do
    s <- get
    case purePop s of
        Just (x, s') -> do
            put s'
            return x
        Nothing -> lift Nothing

peek :: STStack a (Stack a)
peek = get

runStack :: STStack a b -> Stack a -> Maybe (b, Stack a)
runStack = runStateT