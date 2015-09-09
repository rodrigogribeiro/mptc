module Control.Category where

import Base

infixr 9 .:
infixr 1 <<< 
infixr 1 >>> 

class Category cat where 
    idc :: cat a a
    (.:) :: cat b c -> cat a b -> cat a c
    
instance Category (->) where 
    idc = id
    (.:) = (.)
    
-- | Right-to-left composition
(<<<) :: Category cat => cat b c -> cat a b -> cat a c
(<<<) = (.:)

-- | Left-to-right composition
(>>>) :: Category cat => cat a b -> cat b c -> cat a c
f >>> g = g .: f

