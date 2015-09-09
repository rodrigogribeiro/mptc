module SCLoop where

data Bool = True | False

data Maybe a = Nothing | Just a

class SC a where 
  f :: a -> ()

instance SC ()

class SC a => A a b where 
  op :: a -> b -> ()
  op x _ = f x

instance A a b => A a [b]
-- dfun1 :: \d::(A a b) -> DA (sc d)

instance SC a  => A a (Maybe b)
-- dfun2 :: \d::SC a -> DA d

foo = op () ([Just True])
