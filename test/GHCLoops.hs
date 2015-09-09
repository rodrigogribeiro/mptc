{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, EmptyDataDecls  #-}
module Main where 

data A
data B

data IO a 

data Bool = True | False

return :: a -> IO a
	
class Id a b | a -> b, b -> a
	
instance Id A A
instance Id B B

loop :: Id A B => Bool
loop = True

f :: Bool 
f = loop
	
main :: IO () 
main = return ()
