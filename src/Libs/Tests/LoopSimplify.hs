{-#LANGUAGE FlexibleInstances, UndecidableInstances, NoMonomorphismRestriction#-}

module LoopSimplify where

class C a where
  f :: a -> a

--instance D a => C a
instance C a => D a

class D a where
  d :: a -> a
  

instance C [a] => C a 

undefined :: a

g = d undefined
