module SimpleTest where


(+) :: Int -> Int -> Int
(>=) :: Int -> Int -> Bool

data Bool = True | False

otherwise = True

class F a where
   f :: a -> (Int, Int)
 
instance F Double
instance F Float

class Num a
instance Num Int

test x 
    | n >= 0 = m + n   
    | otherwise = m + y 
    where 
       (m,n) = f x 
       (y,y') = f x
   
