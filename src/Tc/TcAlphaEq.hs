
module Tc.TcAlphaEq where

import Control.Monad (liftM, liftM2, zipWithM)
import Language.Haskell.Exts

import Tc.Class
import Tc.TcMonad

-- this module implements algorithms for testing if
-- two types are alpha-equivalent. the algorithm is
-- essentially boilerplate code.

alphaEqM :: Alpha t => t -> t -> TcM Bool
alphaEqM t1 t2 = return (alphaEq t1 t2)

class Alpha t where
   alphaEq :: t -> t -> Bool
   
instance Alpha a => Alpha [a] where
   alphaEq ts ts'
        | length ts == length ts' = and (zipWith alphaEq ts ts')
        | otherwise = False  
        
instance Alpha Type where
   alphaEq (TyVar _) (TyVar _) = True
   alphaEq (TyCon c) (TyCon c') = (c == c')
   alphaEq (TyParen t) (TyParen t') = alphaEq t t'
   alphaEq (TyInfix l n r) (TyInfix l' n' r')
        | n == n' = (&&) (alphaEq l l') (alphaEq r r')   
   alphaEq (TyApp l r) (TyApp l' r')
        = (&&) (alphaEq l l') (alphaEq r r')
   alphaEq (TyList t) (TyList t') 
        = alphaEq t t'
   alphaEq (TyTuple _ ts) (TyTuple _ ts')
        | length ts == length ts' = alphaEq ts ts'
        | otherwise = False
   alphaEq (TyFun l r) (TyFun l' r')
        = (&&) (alphaEq l l') (alphaEq r r')
   alphaEq (TyForall _ ctx t) (TyForall _ ctx' t')
        = (&&) (alphaEq ctx ctx') (alphaEq t t')
   alphaEq _ _ = False

instance Alpha Asst where
   alphaEq (ClassA n ts) (ClassA n' ts')
            | n == n' = alphaEq ts ts'
            | otherwise = False
   alphaEq (InfixA l n r) (InfixA l' n' r')
            | n == n' = (&&) (alphaEq l l') (alphaEq r r')
            | otherwise = False
   alphaEq _ _ = False  
   
instance Alpha Inst where
   alphaEq (Inst n ts ss) (Inst n' ts' ss')
            | n == n' = (&&) (alphaEq ts ts') (alphaEq ss ss')
            | otherwise = False     
