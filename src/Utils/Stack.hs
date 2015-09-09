
module Utils.Stack(Stack,
                   emptyStack,
                   push,
                   pop,
                   peek,
                   singleton,
                   isEmpty,
                   toList,
                   fromList,
                   stackSize) where

import Utils.ErrMsg hiding (isEmpty)

-- this module implements a simple stack
-- using a list.

data Stack a = Stack { unStack :: [a], s_size :: Int }
                  deriving (Eq, Ord, Show)
                  
                  
-- creating a empty stack

emptyStack :: Stack a
emptyStack = Stack [] 0

-- pushing a element   

push :: a -> Stack a -> Stack a
push x (Stack xs n) = Stack (x:xs) (n + 1)

singleton x = push x emptyStack

-- poping a element

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack _ 0) = Nothing
pop (Stack (x:xs) n) = Just (x, Stack xs (n - 1))

-- get the top element

peek :: String -> Stack a -> a
peek _ (Stack (x:_) _) = x
peek s _               = emptyStackPanic s

-- converting to a list

toList :: Stack a -> [a]
toList (Stack xs _) = xs

fromList xs = Stack xs (length xs)

-- checking if a stack is empty

isEmpty :: Stack a -> Bool
isEmpty (Stack _ 0) = True
isEmpty _           = False

-- getting the size of a stack

stackSize :: Stack a -> Int
stackSize (Stack _ n) = n               