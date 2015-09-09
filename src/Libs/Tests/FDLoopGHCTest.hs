module FDLoop where

import Base

class C a b where f :: a -> b

newtype T a = T a

instance (C a b, Eq b) => Eq (T a) where 
   x == y = undefined

g x = (undefined :: d -> d -> d -> ()) (T x) (f x)  (undefined :: Eq e => e)
