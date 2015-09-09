module M where

data Bool = True | False

(&&) :: Bool -> Bool -> Bool


class C a where
  c:: a -> a -> Bool
class D a where
  d:: a -> a -> Bool

instance C Bool where
  c = (&&)

instance D Bool where
  d = (&&)

instance (C a, D a) => C [a] where
  c []    []    = True
  c (a:x) (b:y) = let cab = c a b
                      cxy = c x y
                   in if d a b then cab && cxy else cab || cxy
  c _     _     = False

--c1 = c [True,True] [True,False]
f x = c [[[x]]]
