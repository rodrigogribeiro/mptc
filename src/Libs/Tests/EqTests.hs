module EqTests where

import BuiltIn

data Bool = True | False

(==) :: Int -> Int -> Bool
x == y = primEqInt x y

(==) :: Float -> Float -> Bool
x == y = primEqFloat x y

