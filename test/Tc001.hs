module Simple where

class P a b c where
   p :: a -> b -> c

instance P a b c => P b a c

f x = p x x
