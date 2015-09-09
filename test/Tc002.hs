{-#LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances,FlexibleContexts #-}

module Tc002 where

--import Data.IORef

data IO a

class F f where
  m :: (a -> a) -> f a -> f a

class M m a where
  mm :: (a -> a) -> m -> IO ()

instance (F f, M m (f a)) => M m a where
  mm f v = mm (m f) v
