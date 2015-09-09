
module PreludeBase where

import PreludeBuiltIn
import PreludeNumeric

-- Ratio type

data (Integral a) => Ratio a = !a :% !a  deriving (Eq)

type Rational = Ratio Integer

-- Boolean type

data Bool = True | False deriving (Eq, Ord, Enum, Read, Show, Bounded)

-- Maybe type

data Maybe a = Nothing | Just a deriving (Eq, Ord, Read, Show)

-- Boolean functions

(&&) :: Bool -> Bool -> Bool 
True && x  = x
False && _ = False

(||) :: Bool -> Bool -> Bool
True || _  = True
False || x = x

not :: Bool -> Bool
not True  = False
not False = True

otherwise :: Bool
otherwise = True 

-- Function type

id :: a -> a
id x = x

-- constant function

const :: a -> b -> a
const x _ = x

-- function composition

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($) :: (a -> b) -> a -> b
f $ x  = f x

($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x

seq :: a -> b -> b
seq _ y = y

-- Equality and Ordering

class Eq a where
    (==), (/=) :: a -> a -> Bool
    
    x == y = not (x /= y)
    x /= y = not (x == y)
    
-- Ordering data type

data Ordering = LT 
              | EQ 
              | GT
              deriving (Eq, Ord, Enum, Read, Show, Bounded)    
    
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min :: a -> a -> a
    
class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a
    fail   :: String -> m a
    
class Functor f where 
    fmap :: (a -> b) -> f a -> f b
    
type ReadS a = String -> [(a,String)]
type ShowS = String -> String

class Read a where 
    readsPrec :: Int -> ReadS a
    readList :: ReadS [a]
    
class Show a where 
    showsPrec :: Int -> a -> ShowS 
    show :: a -> String
    showList :: [a] -> ShowS

   
-- Enumeration and Bounded Classes

class Enum a where  
    succ, pred     :: a -> a
    toEnum         :: Int -> a
    fromEnum       :: a -> Int
    enumFrom       :: a -> [a]
    enumFromThen   :: a -> a -> [a]
    enumFromTo     :: a -> a -> [a]
    enumFromThenTo :: a -> a -> a -> [a]

    
class Bounded a where
    minBound :: a
    maxBound :: a
    
-- error related things

error :: String -> a
error = primError

undefined :: a
undefined = error "Prelude.undefined"
   