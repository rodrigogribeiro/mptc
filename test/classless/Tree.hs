module Tree where

data Tree a = Leaf | Node a (Tree a) (Tree a)

map :: (a -> b) -> Tree a -> Tree b
map f Leaf = Leaf
map f (Node v l r) = Node (f v) (map f l) (map f r)
