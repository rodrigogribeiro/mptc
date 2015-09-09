{-#LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies, UndecidableInstances#-}

module TesteMultFin where

--import Base
import Prelude ((*),map,Int,Num,Bool(..),Eq(..))

type Vec a = [a]

class Mult a b c | a b -> c where
   (.*.) :: a -> b -> c

instance Mult Int Int Int --where
--   x .*. y = x * y

instance Mult a b c => Mult a (Vec b) (Vec c) --where
--   x .*. y = map (x *) y

f b x y = if b then x .*. [y] else y

--g :: Vec Int
g = (f True 2 3) == 6
