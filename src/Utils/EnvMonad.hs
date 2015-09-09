
module Utils.EnvMonad(EnvM, 
                      block, 
                      run,
                      handle,
                      defaultHandle,
                      module Control.Monad.State,
                      module Control.Monad.Error) where

import Control.Monad.Trans
import Control.Monad.State (put,get, gets, StateT, runStateT, modify)
import Control.Monad.Error

-- definition of a monad with support of
-- error and state 

type EnvM a b = ErrorT String (StateT a IO) b

-- verify useful operation to block operations
-- INVARIANT: forall e, f' (f e) = e 

block :: (a -> a) -> (a -> a) -> EnvM a b -> EnvM a b
block f f' m 
    = do
        e <- get
        put (f e)
        x <- m
        e' <- get
        put (f' e')
        return x
                 
-- running a Env Monad

run :: a -> EnvM a b -> IO (Either String b, a)
run e m = runStateT (runErrorT m) e            


-- handling errors on the EnvM monad

handle :: EnvM c a -> b -> b -> EnvM c b
handle m b1 b2 
    = do {
          m ;
          return b1
      } `catchError` (const (return b2))     
                 
defaultHandle :: EnvM c a -> EnvM c (Maybe a)
defaultHandle m  
    = do {
        a <- m ;
        return (Just a)
    } `catchError` (const (return Nothing))                
