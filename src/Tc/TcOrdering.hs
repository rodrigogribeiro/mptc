
module Tc.TcOrdering where

import Control.Monad(liftM, liftM2)
import Control.Monad.Error (catchError)
import Language.Haskell.Exts

import Tc.Assumption
import Tc.TcMonad
import Tc.TcSubst

import Utils.ErrMsg

-- this module defines a type class for type ordering

class TyOrd t where
    (<=:), (<:), (>:), (=:), (>=:) :: t -> t -> TcM Bool
    
    t <: t' = liftM2 (&&) (t <=: t') (liftM not (t' <=: t))    
    t =: t' = liftM2 (&&) (t <=: t') (t' <=: t)
    t >: t' = liftM not (t <=: t')
    t >=: t' = liftM not (t <: t')
    
instance TyOrd t => TyOrd [t] where
    ts1 <=: ts2 = leq ts1 ts2 `catchError` (const $ return False)
                 where
                   leq [] [] = return True
                   leq [] _ = return False
                   leq _ [] = return False
                   leq (t:ts) (t':ts') = liftM2 (&&) (t <=: t') (leq ts ts')        
    
instance TyOrd Asst where
   (ClassA n ts) <=: (ClassA n' ts')
        | n == n' = ts <=: ts'
        | otherwise = return False
   (InfixA l n r) <=: (InfixA l' n' r')
        | n == n' = [l,r] <=: [l',r']
        | otherwise = return False
   _ <=: t' = unsupportedExpErrMsg t'  
   
instance TyOrd Assumption where
    (_ :>: t) <=: (_ :>: t') = t <=: t'                   

instance TyOrd Type where
   v@(TyVar _) <=: t
        | not (isFree v) = return True
        | isFree v && isQuantifiedVar t = return False
        | otherwise = do { _ <- match v t ; return True} 
                        `catchError` (const $ return False)
   (TyForall _ ctx t) <=: (TyForall _ ctx' t') 
        = liftM2 (&&) (ctx <=: ctx') (t <=: t')   
   (TyTuple _ ts) <=: (TyTuple _ ts') 
        = ts <=: ts'
   (TyList t) <=: (TyList t') 
        = t <=: t'
   (TyApp l r) <=: (TyList t)
        | isList l = (TyApp l r) <=: (toListApp t)
        | otherwise = return False
   (TyList t) <=: (TyApp l r)
        | isList l = (TyApp l r) <=: (toListApp t)
        | otherwise = return False   
   (TyApp l r) <=: (TyApp l' r') 
        = liftM2 (&&) (l <=: l') (r <=: r')   
   (TyParen t) <=: (TyParen t') 
        = t <=: t'
   (TyCon c) <=: (TyCon c')
        = return (c == c')
   (TyInfix l n r) <=: (TyInfix l' n' r')
        | n == n' = liftM2 (&&) (l <=: l') (r <=: r')
        | otherwise = return False                            
   (TyFun l r) <=: (TyFun l' r') 
        = liftM2 (&&) (l <=: l') (r <=: r')                        
   _ <=: _ = return False   
                 
isQuantifiedVar (TyVar (Ident n)) = head n /= '$'
isQuantifiedVar _ = False

isVar (TyVar _) = True
isVar _ = False                                    