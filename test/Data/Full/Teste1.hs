{-# LANGUAGE MultiParamTypeClasses #-}

module Tests.Data.Full.Teste1 where


-- test module for collecting
-- data types, class and instances
-- information

-- data types

data MyList a = Nil | Cons a (MyList a)
                deriving (Eq, Ord)
                

data Rec = Person {
              name :: String,
              age  :: Int
           } | RecCon String Int
           deriving Eq
           
-- classes

class MyEq a b where
    (=:=) :: a -> b -> Bool
    
    x =:= y = False
    
instance MyEq Int Float where
    x =:= y = False              
                           
