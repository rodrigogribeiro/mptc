module TestMult where

import Base

class Mul a b c where
   (.*.) :: a -> b -> c

instance Mul Int Int Int where
   (.*.) = (*)

instance Mul Int Float Float where
   x .*. y = fromIntegral x * y

instance Mul a b c => Mul a [b] [c] where
   x .*. y = map (x .*.) y

f = \b x y -> if b then x .*. [y] else y

g = f True 1 []

h = g == [1]
