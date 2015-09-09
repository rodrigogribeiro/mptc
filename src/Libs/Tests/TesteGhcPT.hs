
module TesteGhcPT where


class Foo a b where
    foo :: a -> b -> Int
    
instance Foo Int a
instance Foo a b => Foo [a] b 
  
g y = let    
        h :: c -> Int 
        h x = foo y x   
      in h y   
         
  