{-#LANGUAGE MultiParamTypeClasses#-}

module Duggan where

import Base

class Foo a b where
   foo :: a -> b -> Int

instance Foo Int Float where 
   foo x y = 0

instance Foo a b => Foo [a] [b] where
   foo (x:_) (y:_) = foo x y

g x y = (foo [x] y) + (foo [y] x)
