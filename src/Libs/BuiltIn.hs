{-#LANGUAGE NoImplicitPrelude#-}
module BuiltIn where

-- some basic synonyms

type FilePath = String
type String = [Char]

data IOError = IOError

data IO a = IO

-- some basic IO operations

primUserError :: String -> a 
primError :: String -> a
primIOError :: IOError -> IO a
primCatch :: String -> IOError 
--primCatch :: IO a -> (IOError -> IO a) -> IO a
primPutChar :: Char -> IO ()
primGetChar :: IO Char
primGetContents :: IO String
primReadFile :: FilePath -> IO String
primWriteFile :: FilePath -> IO String
primAppendFile :: FilePath -> String -> IO ()

-- primite seq

primSeq :: a -> b -> b

-- operations over type int

primPlusInt :: Int -> Int -> Int
primMinusInt :: Int -> Int -> Int
primMultInt :: Int -> Int -> Int
primNegateInt :: Int -> Int
primGeInt :: Int -> Int -> Bool
primLeInt :: Int -> Int -> Bool
primEqInt :: Int -> Int -> Bool
primDivInt :: Int -> Int -> Int
primModInt :: Int -> Int -> Int
primQuotInt :: Int -> Int -> Int
primRemInt :: Int -> Int -> Int
primMinInt :: Int
primMaxInt :: Int
primIntegerToInt :: Integer -> Int

-- operations over type char

primEqChar    :: Char -> Char -> Bool
primCharToInt :: Char -> Int
primIntToChar :: Int -> Char
primCharIsUpper :: Char -> Bool
primCharIsLower :: Char -> Bool

-- operations over type Integer

primEqInteger  :: Integer -> Integer -> Bool
primAddInteger :: Integer -> Integer -> Integer
primSubInteger :: Integer -> Integer -> Integer
primMulInteger :: Integer -> Integer -> Integer
primNegInteger :: Integer -> Integer
primQuotInteger :: Integer -> Integer -> Integer
primRemInteger  :: Integer -> Integer -> Integer
primDivInteger  :: Integer -> Integer -> Integer
primModInteger  :: Integer -> Integer -> Integer
primIntToInteger :: Int -> Integer

-- operations over types float and double

primEqFloat   :: Float -> Float -> Bool
primEqDouble  :: Double -> Double -> Bool
primAddFloat       :: Float -> Float -> Float
primSubFloat       :: Float -> Float -> Float
primMulFloat       :: Float -> Float -> Float
primNegFloat       :: Float -> Float
primIntToFloat     :: Int -> Float
primIntegerToFloat :: Integer -> Float

primAddDouble       :: Double -> Double -> Double
primSubDouble       :: Double -> Double -> Double
primMulDouble       :: Double -> Double -> Double
primNegDouble       :: Double -> Double
primIntToDouble     :: Int -> Double
primIntegerToDouble :: Integer -> Double

primDivideFloat   :: Float -> Float -> Float
primRecipFloat    :: Float -> Float
primDoubleToFloat :: Double -> Float
primFloatToDouble :: Float -> Double

primDivideDouble :: Double -> Double -> Double
primRecipDouble :: Double -> Double
        
primSinFloat   :: Float -> Float
primCosFloat   :: Float -> Float
primTanFloat   :: Float -> Float
primAsinFloat  :: Float -> Float
primAcosFloat  :: Float -> Float
primAtanFloat  :: Float -> Float
primExpFloat   :: Float -> Float
primLogFloat   :: Float -> Float
primSqrtFloat  :: Float -> Float

primSinDouble   :: Double -> Double
primCosDouble   :: Double -> Double
primTanDouble   :: Double -> Double
primAsinDouble  :: Double -> Double
primAcosDouble  :: Double -> Double
primAtanDouble  :: Double -> Double
primExpDouble   :: Double -> Double
primLogDouble   :: Double -> Double
primSqrtDouble  :: Double -> Double

primIsIEEE  :: Bool
primRadixDoubleFloat  :: Int

primIsNaNFloat  :: Float -> Bool
primIsNegativeZeroFloat  :: Float -> Bool
primIsDenormalizedFloat  :: Float -> Bool
primIsInfiniteFloat  :: Float -> Bool
primDigitsFloat  :: Int
primMaxExpFloat  :: Int
primMinExpFloat  :: Int
primDecodeFloat  :: Float -> (Integer, Int)
primEncodeFloat  :: Integer -> Int -> Float
primAtan2Float   :: Float -> Float -> Float

primIsNaNDouble  :: Double -> Bool
primIsNegativeZeroDouble  :: Double -> Bool
primIsDenormalizedDouble  :: Double -> Bool
primIsInfiniteDouble  :: Double -> Bool
primDigitsDouble  :: Int
primMaxExpDouble  :: Int
primMinExpDouble  :: Int
primDecodeDouble  :: Double -> (Integer, Int)
primEncodeDouble  :: Integer -> Int -> Double
primAtan2Double   :: Double -> Double -> Double

primShowFloat :: Float -> String
primShowDouble :: Double -> String

-- I/O operations

primbindIO :: IO a -> (a -> IO b) -> IO b
primretIO :: a -> IO a
primExitWith :: Int -> a
