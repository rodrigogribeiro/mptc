
module Tc.TcWellformed where

import Control.Monad.Trans
import Control.Monad (liftM, liftM2, foldM)
import Data.List ((\\), intersect, union)
import Language.Haskell.Exts

import Tc.Assumption
import Tc.TcAlphaEq
import Tc.TcCriteria(anyM)
import Tc.Class
import Tc.TcMonad
import Tc.TcOrdering
import Tc.TcSat
import Tc.TcSubst

import Utils.Id
import Utils.ErrMsg
import Utils.Debug

-- this module implements the algorithms for:
-- 1. Checking well-formedness of a type

wfAssump :: Assumption -> TcM Assumption
wfAssump (i :>: t) 
    = do 
         s' <- getSubst
         vs <- ftvM 
         s <- wf (apply s' t) (apply s' vs)
         let 
            (TyForall Nothing _ ty) = apply s' t
            f (s, ctx) = return (i :>: TyForall Nothing ctx (apply s' ty))
         maybe (notWellFormedTypeError t) f s

                
-- a type class for representing the algorithms. the result is Just s if 
-- the parameter is well-formed, otherwise: nothing.                 

class Wf t where
    wf :: t -> [TVar] -> TcM (Maybe (Subst, Context)) 
    
instance Wf Type where
    wf ty@(TyForall _ k t) vs 
    	| null k0 = check solved >> return (Just (nullSubst,k \\ solved))
    	| otherwise 
    		= do {
				ss <- sat k0 ;
				case ss of {
						-- dirty hack ahead! If is well-formed, update the substitution on state
				   [s] -> if null (fv (apply s k0)) then extSubst s >> return (Just (s,[]))
		    		        else notWellFormedTypeError ty ;   			 
				   _   -> notWellFormedTypeError ty    			 										
				}
			  }
    	where
          solved = [d | d <- k, null (fv d)]
          k' = [d | d <- k, null (fv d `intersect` vs)]
          vs' = (fv t) \\ vs
          k0 = (k' \\ (k' |* vs')) `union` solved
    wf _ _ = return Nothing                       
                       
        
type TVar = Type -- INVARIANT: Just type variables       

       
-- functions for implementing the constraint set closure

(|*) :: Context -> [TVar] -> Context
ctx |* v 
	| null (v' \\ v) = ctx'
	| otherwise = ctx |* v'
	where
	   (ctx',v') = (ctx |: v, fv ctx')    

(|:) :: Context -> [TVar] -> Context
ctx |: v = filter (\c -> not (null $ fv c `intersect` v)) ctx    


-- checks if solved constraints have a corresponding declared instances

check :: [Asst] -> TcM Bool            
check = foldM step True
        where
           step ac (InfixA l n r) = step ac (ClassA n [l,r])
           step ac (ClassA n ts) 
              = do
                  is <- getInstances (toId n)
                  liftM (ac &&) (anyM (alphaEqM ts . instparameters) is)
                  
verify :: Asst -> TcM (Maybe Subst)
verify k = do 
              b <- check [k]
              if b then return (Just nullSubst)
                else noSatInstanceError k                  
             
-- some auxiliar functions 

subseteq :: Eq a => [a] -> [a] -> Bool
subseteq xs ys = all (`elem` ys) xs             
