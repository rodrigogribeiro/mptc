
module PreludeNumeric where

import PreludeBuiltIn
import PreludeBase

-- Numeric Classes

class (Eq a, Show a) => Num a where
    (+), (-), (*) :: a -> a -> a
    negate        :: a -> a
    abs, signum   :: a -> a
    fromInteger   :: Integer -> a
    
instance Num Int where
    (+) = primPlusInt
    (-) = primMinusInt
    (*) = primMultInt
    negate x = primNegateInt x
    abs n = if primGeInt n 0 then n 
              else primNegateInt n
    signum n 
        | primLeInt n 0 = primNegateInt 1
        | primEqInt n 0 = 0
        | otherwise = 1       
    
class (Num a, Ord a) => Real a where
    toRational :: a -> Rational
    
class (Real a, Enum a) => Integral a where
    quot, rem       :: a -> a -> a
    div, mod        :: a -> a -> a
    quotRem, divMod :: a -> a -> (a,a)
    toInteger       :: a -> Integer
    
                 

class Num a => Fractional a where
    (/)          :: a -> a -> a
    recip        :: a -> a
    fromRational :: a -> Rational
    
   

class Fractional a => Floating a where
    pi                  :: a
    exp, log, sqrt      :: a -> a
    (**), logBase       :: a -> a -> a
    sin, cos, tan       :: a -> a
    asin, acos, atan    :: a -> a
    sinh, cosh, tanh    :: a -> a
    asinh, acosh, atanh :: a -> a
    
   
class (Real a, Fractional a) => RealFrac a where
    properFraction :: Integral b => a -> (b, a)
    truncate, round :: Integral b => a -> b
    ceiling, floor  :: Integral b => a -> b
    

    
class (RealFrac a, Floating a) => RealFloat a where
    floatRadix  :: a -> Integer
    floatDigits :: a -> Int 
    floatRange  :: a -> (Int, Int)
    decodeFloat :: a -> (Integer, Int)
    encodeFloat :: Integer -> Int -> a
    exponent    :: a -> Int
    significand :: a -> a 
    scaleFloat  :: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, 
         isIEEE :: a -> Bool
    atan2 :: a -> a -> a 