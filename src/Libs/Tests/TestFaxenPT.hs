module TestFaxenPT where

--import Base

class IsNil a where
   isNil :: a -> Bool

instance IsNil [b] where
   isNil [] = True
   isNil _  = False

f x y = let g = isNil
          in (g x, g y)
