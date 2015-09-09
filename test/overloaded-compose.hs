{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, NoMonomorphismRestriction #-}

class Num a

(+) :: Int -> Int -> Int
x + y = x

data IO a

type String = [Char]

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

filter :: (a -> Bool) -> [a] -> [a]

words :: String -> [String]

class Eq a where
  (==) :: a -> a -> Bool

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

print :: Show a => a -> String

class Show a where
    show :: a -> String

instance Show Int

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

class Compose a b c e f where
  (<.>) :: (b -> c) -> (a -> e) -> (a -> f)

instance Compose a b c b c where
  (<.>) = (.)

instance Monad m => Compose a b c (m b) (m c) where
   (<.>) f g a = g a >>= (return . f)

count :: String -> String -> Int
count w = length <.> filter (==w) <.> words

main :: IO ()
main = ((print <.> count) "1") "1 2 3 1 4"
