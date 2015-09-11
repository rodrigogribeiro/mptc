
module Data.TestCase1KindInference where

class F a where
  f :: a b -> b

class Category cat where
   id :: cat a a
   (.) :: cat b c -> cat a b -> cat a c
   
data Tree a = Leaf a | Node a (Tree a) (Tree a)

type ITree = Tree    

data Either a b = Left a | Right b
  
class Category a => Arrow a where
   arr :: (b -> c) -> a b c
   first :: a b c -> a (b,d) (c,d)
   second :: a b c -> a (d,b) (d,c)
   (***) :: a b c -> a b' c' -> a (b,b') (c,c')
   (&&&) :: a b c -> a b c' -> a b (c,c')

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

class Arrow a => ArrowZero a where
        zeroArrow :: a b c
       
class ArrowZero a => ArrowPlus a where
        (<+>) :: a b c -> a b c -> a b c
        
class Arrow a => ArrowApply a where
        app :: a (a b c, b) c
        
class Arrow a => ArrowLoop a where
        loop :: a (b,d) (c,d) -> a b c                 
