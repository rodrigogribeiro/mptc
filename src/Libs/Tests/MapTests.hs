module MapTests where

import Base

data Tree a = Leaf | Branch a (Tree a) (Tree a)

data List a = Nil | Cons a (List a)

map :: (a -> b) -> Tree a -> Tree b
map f Leaf = Leaf
map f (Branch v l r) = Branch (f v) (map f l) (map f r)

map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons v l) = Cons (f v) (map f l)
