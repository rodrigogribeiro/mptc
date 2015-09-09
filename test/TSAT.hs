{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances#-}
module TSAT where

class D a b where
   d :: a -> b

class C a b where
   c :: a -> b -> Bool

instance C Char a => D Char [Bool]
instance D a [a] => C Char a
instance C Char a => D Char a

f = d 'c' -- bug on context reduction?
