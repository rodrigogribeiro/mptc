
module Tc.TcMonad where

import Control.Monad.Trans
import Data.List((\\),union, nub)
import Language.Haskell.Exts 

import Tc.Assumption
import Tc.Class 
import Tc.TcEnv
import Tc.TcInst
import Tc.TcSubst
import Tc.TcLabel

import Utils.Env
import Utils.EnvMonad
import Utils.ErrMsg hiding (empty)
import Utils.Id
import Utils.Stack

-- this module defines the tc monad type
-- and some operations over it.

type TcM a = EnvM TcEnv a

-- some operations over the Tc monad

getInstances :: Id -> TcM [Inst]
getInstances n 
    = do 
       c <- gets (lookupEnv n . classenv)
       maybe (return []) (mapM freshInst . instances) c
       
getVarEnv :: TcM (Stack VarEnv)
getVarEnv = gets varenv       
       
getClass :: Id -> TcM Class
getClass n
    = do
        c <- gets (lookupEnv n . classenv)
        maybe (classNotDefinedError (unid n)) freshInst c       

       
getSubst :: TcM Subst
getSubst = gets subst


extSubst :: Subst -> TcM ()
extSubst s = modify (\e -> e{ subst = s @@ (subst e)})    


freshM :: TcM Int 
freshM = do
            e <- get
            let i = fresh e
            put (e{fresh = i + 1})
            return i
                        
-- getting labels from a record constructor

lookupLabels :: Id -> TcM [Label]
lookupLabels i 
    = do
        e <- gets (labelsFrom i)
        maybe (notDefinedError (unid i)) return e            

-- typing context related functions

lookupGamma :: Id -> TcM Assumption
lookupGamma i = do
                  e <- gets (lookupVar i)
                  maybe (notDefinedError (unid i)) return e
                  
-- including new assumptions

extendGamma :: [Assumption] -> TcM a -> TcM a
extendGamma as t 
    = block f f' t
      where
        is = map (\(i :>: _) -> i) as
        g (i :>: t) ac = insert i (i :>: t) ac
        f s = s{varenv = push p (varenv s)}
        p = foldr g empty as
        f' s = s {varenv = maybe (emptyStackPanic "Tc.TcMonad.extendGamma")
                                 snd (pop $ varenv s)}                    
            
-- generating a new fresh type variable

newFreshVar :: TcM TypeVar
newFreshVar = do
                n <- liftM (('$':) . show) freshM
                return (TyVar (Ident n))
                
-- a instantiation function inside tc monad

freshInst :: (Substitutable t, Instantiate t) => t -> TcM t
freshInst t = do
                let
                   vs = bv t
                vs' <- mapM (const newFreshVar) vs
                return (instantiate vs vs' t)
                
-- free type variables in context

ftvM :: TcM [TypeVar]
ftvM = getSubst >>= \s -> liftM (apply s) (gets fv)
        
-- quantification of a type                        
                
quantify :: Context -> Type -> TcM Type
quantify ctx t 
    = do
        ns <- ftvM
        let
          s = zip ((fv ctx `union` fv t) \\ ns) allBoundVars
          t' = TyForall Nothing (apply s ctx) (apply s t)
        return t'

allBoundVars :: [TypeVar]
allBoundVars = [f [x] | x <- ['a'..'z']] ++ 
                  [f (x : show i) | i <- [1..], x <- ['a'..'z']]
               where
                  f x = TyVar (Ident x)
                  
-- quantify classes and instances for constraint generations for
-- overloaded bindings

quantifyClass :: Class -> TcM Class 
quantifyClass c@(Class n ts ss as is)
    = do
         vs <- liftM (\v -> fv c \\ v) ftvM
         let s = zip vs allBoundVars
         is' <- mapM quantifyInst is
         let c' = apply s c 
         return (c' {instances = is'})        
         
quantifyInst :: Inst -> TcM Inst 
quantifyInst i@(Inst n ts ss)
    = do
        vs <- liftM (\v -> fv i \\ v) ftvM
        let s = zip vs allBoundVars
        return (apply s i)         
                        
                       
-- unification functions

unify :: (MGU t) => t -> t -> TcM Subst
unify t t' = unify1 True t t'


unify1 :: (MGU t) => Bool -> t -> t -> TcM Subst
unify1 b t t' = do
                  s <- getSubst
                  s' <- mgu (apply s t) (apply s t')
                  when b (extSubst s')   
                  return s' -- XXX may break  
                  
matchOk :: (Matchable t) => t -> t -> TcM Bool
matchOk t1 t2 = handle (match t1 t2) True False     

matchfy :: (Matchable t) => t -> t -> TcM Subst
matchfy t1 t2 
	= matchfy1 False t1 t2

matchfy1 :: (Matchable t) => Bool -> t -> t -> TcM Subst
matchfy1 b t1 t2 
    = do
		s  <- getSubst
		s' <- match (apply s t1) (apply s t2)
		when b (extSubst s')
		return s'
                  
-- a type class for the unification

class Substitutable t => MGU t where
    mgu :: t -> t -> TcM Subst
    
instance (Pretty t, MGU t) => MGU [t] where
    mgu [] [] = return nullSubst
    mgu (t:ts) (t':ts') 
        = do
            s <- mgu t t'
            s' <- mgu (apply s ts) (apply s ts')
            return (s' @@ s)
    mgu t t' = differentSizeListUnifyError t t'
    
instance MGU Asst where
    mgu c1@(ClassA n ts) c2@(ClassA n' ts')
        | n == n' = mgu ts ts'
        | otherwise = unificationError c1 c2     
    mgu c1@(InfixA l n r) c2@(InfixA l' n' r')
        | n == n' = mgu [l,r] [l',r']
        | otherwise = unificationError c1 c2             
    
instance MGU Type where
    mgu (TyForall _ _ ty) (TyForall _ _ ty')
        = mgu ty ty'            
    mgu (TyFun l r) (TyFun l' r')
        = do
            s <- mgu l l'
            s' <- mgu (apply s r) (apply s r')
            return (s' @@ s)
    mgu (TyTuple _ ts) (TyTuple _ ts')
        = mgu ts ts'
    mgu (TyTuple x ts) (TyApp l r)
        | isTuple l = mgu (toTupleApp ts) (TyApp l r)
        | otherwise = unificationError (TyTuple x ts) (TyApp l r)        
    mgu (TyList t) (TyList t')
        = mgu t t'
    mgu (TyList t) (TyApp l r)
        | isList l = mgu (toListApp t) (TyApp l r)
        | otherwise =  unificationError (TyList t) (TyApp l r) 
    mgu (TyApp l r) (TyList t)
        | isList l = mgu (toListApp t) (TyApp l r)
        | otherwise = unificationError (TyApp l r) (TyList t)    
    mgu (TyApp l r) (TyTuple x ts)
        | isTuple l = mgu (toTupleApp ts) (TyApp l r)
        | otherwise = unificationError (TyTuple x ts) (TyApp l r)                  
    mgu (TyApp l r) (TyApp l' r')
        = do
            s <- mgu l l'
            s' <- mgu (apply s r) (apply s r')
            return (s' @@ s)
    mgu (TyCon n) (TyCon n')
        | n == n' = return nullSubst
        | otherwise = unificationError n n'
    mgu (TyParen t) (TyParen t')
        = mgu t t'
    mgu (TyParen t) t' = mgu t t'
    mgu t (TyParen t') = mgu t t'        
    mgu (TyInfix l n r) (TyInfix l' n' r')
        | n == n' = do
                        s <- mgu l l'
                        s' <- mgu (apply s r) (apply s r')
                        return (s' @@ s)
        | otherwise = unificationError n n'   
    mgu v@(TyVar _) t 
        | isFree v = varBind v t
        | otherwise = boundVariableInUnifyError v t    
    mgu t v@(TyVar _)
        | isFree v = varBind v t
        | otherwise = boundVariableInUnifyError v t    
    mgu t t' = unificationError t t'
    
  
-- type class for matching operation

class Substitutable t => Matchable t where
    match :: t -> t -> TcM Subst    
    
instance (Substitutable t, Pretty t, Matchable t) => Matchable [t] where
    match [] [] = return nullSubst
    match (t:ts) (t':ts') 
        = do
            s <- match t t'
            s' <- match (apply s ts) (apply s ts')
            maybe (matchingError t t') return (merge s' s)
    match t t' = differentSizeListUnifyError t t'        
    
instance Matchable Asst where
    match t@(ClassA n ts) t'@(ClassA n' ts')
        | n == n' = match ts ts'
        | otherwise = matchingError t t'
    match t@(InfixA l n r) t'@(InfixA l' n' r')
        | n == n' = match [l,r] [l',r']
        | otherwise = matchingError t t'
        
instance Matchable Type where
    match t@(TyForall _ ctx ty) t'@(TyForall _ ctx' ty')
        = do
            s <- match ctx ctx'
            s' <- match (apply s ty) (apply s ty')
            maybe (matchingError t t') return (merge s' s)
    match t@(TyFun l r) t'@(TyFun l' r')
        = do
            s <- match l l'
            s' <- match (apply s r) (apply s r')
            maybe (matchingError t t') return (merge s' s)
    match t@(TyTuple _ ts) t'@(TyTuple _ ts')
        = match ts ts'
    match (TyTuple b ts) (TyApp l r)
        | isTuple l = match (toTupleApp ts) (TyApp l r)
        | otherwise = matchingError (TyTuple b ts) (TyApp l r)
    match (TyApp l r) (TyTuple b ts)
        | isTuple l = match (TyApp l r) (toTupleApp ts)
        | otherwise = matchingError (TyTuple b ts) (TyApp l r)                
    match (TyList t) (TyList t')
        = match t t'
    match (TyList t) (TyApp l r)
        | isList l = match (toListApp t) (TyApp l r)
        | otherwise =  matchingError (TyList t) (TyApp l r)
    match (TyApp l r) (TyList t)
        = match (TyApp l r) (toListApp t)               
    match t@(TyApp l r) t'@(TyApp l' r')                    
        = do
            s <- match l l'
            s' <- match (apply s r) (apply s r')
            maybe (matchingError t t') return (merge s' s)
    match (TyParen t) (TyParen t')
        = match t t'
    match (TyParen t) t' = match t t'
    match t (TyParen t') = match t t'        
    match t@(TyInfix l n r) t'@(TyInfix l' n' r')
        | n == n' 
            = do
                s <- match l l'
                s' <- match (apply s r) (apply s r')
                maybe (matchingError t t') return (merge s' s)
        | otherwise = matchingError t t'
    match v@(TyVar _) t 
        | isFree v = varBind v t
        | otherwise = boundVariableInUnifyError v t
    match t@(TyCon c) t'@(TyCon c') 
        | c == c' = return nullSubst
        | otherwise = matchingError t t' 
    match t t' = matchingError t t'    
                    

-- binding a variable to a type
    
varBind :: Type -> Type -> TcM Subst
varBind v t
    | t == v = return nullSubst
    | v `elem` fv t = occursCheckError v t
    | otherwise = return (v +-> t)
    
-- some auxiliar functions

isList (TyApp l _) = isList l
isList (TyCon (Special ListCon)) = True
isList _ = False

toListApp t = TyApp (TyCon (Special ListCon)) t   

isTuple (TyApp l _) = isTuple l
isTuple (TyCon (Special (TupleCon b n))) = True
isTuple _ = False

toTupleApp ts = let n = length ts
                  in foldl TyApp (TyCon (Special (TupleCon Boxed n))) ts  
                                     
