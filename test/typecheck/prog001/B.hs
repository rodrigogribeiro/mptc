{-# LANGUAGE MultiParamTypeClasses #-}
module B where
import A

data Val = Val [Int]

instance Matrix Bool Val
