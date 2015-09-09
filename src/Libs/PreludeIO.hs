module PreludeIO where

--import Base
import BuiltIn

class Num a 

instance Num Int

class Show a where
   show :: a -> String

instance Show Int

putStrLn :: String -> IO ()

print :: Show a => a -> IO ()
print x = putStrLn (show x)
