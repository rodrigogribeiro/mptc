
module Tc.TcSubst where

import Data.List (union, intersect)
import Language.Haskell.Exts

import Tc.Assumption
import Tc.Class

import Utils.ErrMsg

-- this module defines the substitution
-- data type and some functions over it

-- definition of a substitution

-- INVARIANT: left component of the pair is ALWAYS
--            a type variable

type Subst = [(Type, Type)]

nullSubst :: Subst
nullSubst = []

type TypeVar = Type

-- a type class for commom substitution operations

class Substitutable t where
    apply :: Subst -> t -> t
    tv :: t -> [TypeVar]
    
-- some basic instances    
    
instance Substitutable t => Substitutable [t] where
    apply s = map (apply s)
    tv = foldr (union . tv) []  
    
instance Substitutable Asst where
    apply s (ClassA n ts) = ClassA n (apply s ts)
    apply s (InfixA l n r) = InfixA (apply s l) n (apply s r)
    apply s t = unsupportedDeclMsg t
    
    tv (ClassA _ ts) = tv ts
    tv (InfixA l _ r) = tv [l, r]   
    tv t = unsupportedDeclMsg t

instance Substitutable Type where
    apply s (TyForall m ctx t) 
        = TyForall m (apply s ctx) (apply s t)
    apply s (TyFun l r)
        = TyFun (apply s l) (apply s r)
    apply s (TyTuple b ts)
        = TyTuple b (apply s ts)
    apply s (TyList t)
        = TyList (apply s t)
    apply s (TyApp l r)
        = TyApp (apply s l) (apply s r)
    apply s (TyParen t)
        = TyParen (apply s t)
    apply s (TyInfix l n r)
        = TyInfix (apply s l) n (apply s r)
    apply s t@(TyCon _) = t         
    apply s v@(TyVar _) = case lookup v s of
                            Nothing -> v
                            Just t  -> t
    apply s t = unsupportedDeclMsg t                                    

    tv (TyForall _ ctx t) = tv ctx `union` tv t
    tv (TyFun l r) = tv [l, r]
    tv (TyTuple _ ts) = tv ts
    tv (TyList t) = tv t
    tv (TyApp l r) = tv [l, r]
    tv (TyParen t) = tv t
    tv (TyInfix l _ r) = tv [l, r]
    tv (TyCon _) = []
    tv v@(TyVar _) = [v]
    tv t = unsupportedDeclMsg t

instance Substitutable Class where
    apply s (Class n ts ctx as is) 
        = Class n (apply s ts) (apply s ctx) (apply s as) is
    tv (Class _ ts ctx as _) = tv ts `union` tv ctx `union` tv as        
        
instance Substitutable Inst where
    apply s (Inst n ts ctx) 
        = Inst n (apply s ts) (apply s ctx)
    tv (Inst _ ts ctx) = tv ts `union` tv ctx
    
instance Substitutable Assumption where
    apply s (i :>: t) = i :>: (apply s t)
    tv (_ :>: t) = tv t
               
    
-- free and bound variables

fv :: (Substitutable t) => t -> [TypeVar]
fv = filter isFree . tv    

bv :: (Substitutable t) => t -> [TypeVar]
bv = filter (not . isFree) . tv
    
-- substitution composition

infixr 4 @@

(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u,t) <- s2] ++ s1

-- merging substitution for matching

merge :: Subst -> Subst -> Maybe Subst
merge s1 s2 = if ok then Just (s1 ++ s2) else Nothing
              where 
                ok = all (\v -> apply s1 v == apply s2 v)
                            (map fst s1 `intersect` map fst s2) 
                            
-- build a substitution

(+->) :: Type -> Type -> Subst
v +-> t = [(v,t)]          

-- True when the variable is free

isFree (TyVar (Ident n)) = head n == '$'
isFree _ = False                              
