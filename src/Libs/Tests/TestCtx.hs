module TestCtx where

tail [] = []
tail (_:xs) = xs 

class C a where c_method :: a -> Bool

instance C a => C [a]

foo xs = c_method (tail xs) 
 
