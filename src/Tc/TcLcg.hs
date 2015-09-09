
module Tc.TcLcg where

-- this module implements the functions for compute
-- the least commom generalization of two simple
-- types.

import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List
import qualified Data.Map as Map
import Language.Haskell.Exts

import Tc.TcMonad
import Tc.TcSubst

import Utils.ErrMsg
import Utils.Id


type Var = Type -- INVARIANT: Always hold a type variable

-- a historic of possible generalizations

type History = [((Type, Type), Var)]

-- the main lcg function

lcg :: Id -> [Type] -> TcM Type
lcg i ts 
    = do
        (t,_) <- lcg' i ts []
        return t

lcg' :: Id -> [Type] -> History -> TcM (Type, History)
lcg' i [t] h 
    = do 
       t' <- freshInst t
       return (t', h)
lcg' i (t:ts) h 
    = do
        (ctxs, ts') <- liftM (unzip . map splitTy) (mapM freshInst ts)
        (t',h') <- lcgn' ts' h
        (ctxi,ti) <- liftM splitTy (freshInst t)
        (t'', h'') <- lcgp t' ti h'
        let vs = (tv t'') \\ (tv t' ++ tv ti)
            vs' = tv ctxs `union` tv ctxi
            s = gensubst vs' h''
            cs = apply s (concat ctxs ++ ctxi)
        return (TyForall Nothing (nub $ (gen i vs) ++ cs) t'', h'')
lcg' i [] _ = emptyLcgList      


gensubst :: [Var] -> History -> Subst
gensubst vs h
	= foldr step [] h
	   where
		 step ((t1,t2),t) ac
			| t1 `elem` vs || t2 `elem` vs = (t1,t) : (t2,t) : ac
			| otherwise = ac


lcgp :: Type -> Type -> History -> TcM (Type, History)
lcgp t1 t2 h 
    = case lookup (t1,t2) h of
         Just v  -> return (v,h)
         Nothing -> lcgp' t1 t2 h            



lcgp' :: Type -> Type -> History -> TcM (Type, History)
lcgp' t1@(TyVar _) t2@(TyVar _) h
    = return (t1, ((t1,t2),t1):h)
lcgp' t1@(TyVar _) t2 h 
    = do
        v <- newFreshVar
        return (v, ((t1,t2),v):h)
lcgp' t1 t2@(TyVar _) h
    = do
        v <- newFreshVar
        return (v, ((t1,t2),v):h)
lcgp' t1@(TyCon c1) t2@(TyCon c2) h
    | c1 == c2 = return (t1,h)
    | otherwise 
        = do 
            v <- newFreshVar
            return (v, ((t1,t2), v) : h) 
lcgp' t1@(TyFun l1 r1) t2@(TyFun l2 r2) h
    = do
        (lr,h1) <- lcgp l1 l2 h
        (rr,h2) <- lcgp r1 r2 h1
        return (TyFun lr rr,h2)                 
lcgp' (TyApp l1 r1) (TyApp l2 r2) h
    = do
        (lr,h1) <- lcgp l1 l2 h
        (rr,h2) <- lcgp r1 r2 h1
        return (TyApp lr rr,h2)
lcgp' (TyList t) (TyList t') h 
    = do
        (tr, h') <- lcgp t t' h
        return (TyList tr, h')
lcgp' t1@(TyTuple x ts) t2@(TyTuple y ts') h 
    | length ts == length ts'
        = do
            (tr, h') <- lcgzip ts ts' h
            return (TyTuple x tr, h')
    | otherwise 
        = do
            v <- newFreshVar
            return (v, ((t1,t2),v):h)                            
lcgp' t1@(TyApp l r) t2@(TyList t) h 
    = lcgp' (toListApp t) (TyApp l r) h

lcgp' t1@(TyList t) t2@(TyApp l r) h
    = lcgp' (toListApp t) (TyApp l r) h
lcgp' t1@(TyTuple _ ts) t2@(TyApp l r) h
    = lcgp' (toTupleApp ts) (TyApp l r) h
lcgp' t1@(TyApp l r) t2@(TyTuple _ ts) h
    = lcgp' (toTupleApp ts) (TyApp l r) h
lcgp' (TyParen t) (TyParen t') h 
    = do 
        (tf, h') <- lcgp' t t' h
        return (TyParen tf, h')   
lcgp' t1 t2 h
    = do
        v <- newFreshVar
        return (v, ((t1,t2),v):h)
                          
                  
lcgn' :: [Type] -> History -> TcM (Type, History)
lcgn' [t] h = return (t,h)
lcgn' [t1,t2] h = lcgp t1 t2 h
lcgn' (t1:t2:ts) h
    = do
        (ts',h') <- lcgn' ts h
        (t',h'') <- lcgp t1 t2 h' 
        lcgp ts' t' h''
        
lcgzip :: [Type] -> [Type] -> History -> TcM ([Type], History)
lcgzip [] [] h' = return ([], h')
lcgzip (t:ts) (t':ts') h
    = do
        (tr, h') <- lcgp t t' h
        lcgzip ts ts h'
             

gen :: Id -> [Type] -> [Asst]
gen i t 
    | isId i = [ClassA ii t]
    | otherwise = [ClassA i' t]
      where
          i' = UnQual (Ident ("_" ++ (show i)))
          ii = UnQual (Ident n)
          n  = let x = show i in (toUpper (head x) : tail x)

-- lcg of substitutions

lcgs :: [Subst] -> TcM Subst
lcgs ss 
    = do
        let m = Map.toList $ foldr step Map.empty ss 
            step s ac = foldr go ac s
            go (v,t) ac = Map.insertWith (++) v [t] ac       
        foldM (\ac (v,ts) 
                -> do {
                      (t', _) <- lcgn' ts [] ;
                      return ((v,t') : ac)
                   }) nullSubst m

        
-- some auxiliar functions

splitTy :: Type -> (Context, Type)
splitTy (TyForall _ ctx t) = (ctx,t)
splitTy t = ([],t)                                

isId = isLetter . head . show
