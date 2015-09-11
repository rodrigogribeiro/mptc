
module Data.TestCase1TypeSynExpand where


type String1 = [Char]

f :: String1 -> String1
f x = x

data Tree a = Leaf | Node a (Tree a) (Tree a)

type ITree = Tree String1

g :: ITree -> ITree
g x = x

h x = let
        y :: ITree -> ITree
        y = g
      in g Leaf
