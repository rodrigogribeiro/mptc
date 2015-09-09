
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

class Isomorphism a b where
    fw :: a -> b
    bw :: b -> a

instance (Isomorphism a b, Isomorphism b c) => Isomorphism a c where
    fw = fw . fw
    bw = bw . bw
