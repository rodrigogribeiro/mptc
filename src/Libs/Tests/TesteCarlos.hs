{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, NoMonomorphismRestriction, UndecidableInstances#-}

module MyTeste where

--import Prelude ()

class Eq a where
    (==) :: a -> a -> Bool
    
instance Eq Bool    

data Bool = True | False

class F a 
instance F a
class D b 
instance F b => D b
--instance D Bool

class D b => C a b where
    x :: a -> b
    
instance D b => C [a] b

--h = x [1] 

f = x []

g = f == True -- diferença entre unify e match
