{-#LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction#-}
module Tests.Data.Full.Teste2 where

class F a b where
   f :: a -> b
  
   
class O a where
   o :: a 

h = f o

x = h