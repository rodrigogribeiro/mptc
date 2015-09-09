module ListMap where

import Tree

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

id x = x 

teste x = map id x
