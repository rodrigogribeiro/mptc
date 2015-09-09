module Tc.TcClassInstGen(genClassInsts) where

import Control.Monad
import Control.Monad.Trans

import Data.Char (isUpper,toUpper)
import Data.List
import qualified Data.Map as Map

import Language.Haskell.Exts hiding (name)

import Tc.Assumption
import Tc.Class
import Tc.TcLabel
import Tc.TcLcg hiding (gen)
import Tc.TcMonad hiding (unify, getSubst)
import Tc.TcSubst
import Tc.TcEnv
import Tc.Kc.KcDriver hiding (select)
import Tc.Kc.KcEnv 
import Tc.Kc.KcMonad 

import Utils.ErrMsg
import Utils.EnvMonad
import Utils.Id
import qualified Utils.Nameable as N

-- this module provides the functionality of generating class/instances
-- from simple binds declarations, allowing a kind of "class-less" 
-- Haskell

-- top level function
-- it returns the list of generated classes / instances
-- and the respective instance binds

genClassInsts :: ClassEnv -> KcEnv -> LabelEnv -> [Assumption] -> Module -> TcM ([Class],[Decl],KcEnv,[Assumption])
genClassInsts cenv kc e as (Module _ _ _ _ _ _ ds) 
    = do
        let
           sigs = concatMap sig2Assump (filter isTypeSig ds)
        as' <- freshInst (as ++ sigs)
        (cs, env', oversigs) <- genConstraints cenv kc e as'
        binds <- overloaded cs (filter isBind ds)
        return (cs,binds,env',oversigs)
        
-- this function returns the set of overloaded bindings. In order to
-- select, I've used an little hack based on its Id. I know, I know...
-- Its ugly, but I think that will work.        

overloaded :: [Class] -> [Decl] -> TcM [Decl]
overloaded cs ds
    = foldM go [] ds
      where
         ns = map name cs
         go ac d 
            = return (if over d then d : ac else ac)
         over d 
            = (N.name d) `elem` ns || 
               ("_" ++ show (N.name d)) `elem` (map show ns) 
                  
-- this function generates the constraints and returns the associated 
-- assumptions used to generate them.    

genConstraints :: ClassEnv -> KcEnv -> LabelEnv -> [Assumption] -> TcM ([Class], KcEnv, [Assumption])
genConstraints cenv kenv env as
    = do
        (cs, env') <- foldM (gen cenv) ([],kenv) candidates
        return (cs, env', concat candidates)
      where
         candidates = groupById (select env as)

-- generating the constraints for a set of overloaded identifiers
         
gen :: ClassEnv -> ([Class], KcEnv) -> [Assumption] -> TcM ([Class], KcEnv) --- XXX finish fixes in this function
gen cenv (cs, env) as
    = do
        let 
           i = (\(i :>: _) -> i) (head as)
           ts = map (\(_ :>: t) -> t) as
        a <- liftM (i :>:) (lcg i ts)   
        c <- genClass a
        c' <- quantifyClass =<< genInsts a ts c
        -- ugly hack to perform kind inference on generated
        -- constraints   
        (c'',e) <- liftIO (kcConstr env c')        
        return (c'':cs,e) 

-- this function generates the class constraint. It uses the dirty
-- trick of id comparison.
        
genClass :: Assumption -> TcM Class
genClass a@(i :>: (TyForall _ ctx ty))  
    = return (Class i' ts sups [a] [])
      where 
        c@(ClassA n ts) = findConstraint ctx
        i' = toId n
        sups = ctx \\ [c]
        i1 = let x = show i in toId (Ident ((toUpper (head x)) : (tail x)))
        findConstraint [] = lcgWithoutConstraint (show i)
        findConstraint (x:xs)
            | valid x = x
            | otherwise = findConstraint xs 
        -- dirty id based hack, again... :(
        valid x = (idof x) == i1 || (show $ idof x) == ("_" ++ show i)
          
genInsts :: Assumption -> [Type] -> Class -> TcM Class
genInsts (i :>: t) ts c 
    = do
        let (c1,t1) = splitTy t
            ts2 = map splitTy ts
            (c2,t2) = unzip ts2           
        ss <- mapM (match t1) t2   
        let tss = map (flip apply ts') ss
            sups = concat (concatMap (flip apply c2) ss)
            ts' = parameters c
            is = map (\ts -> Inst (name c) ts sups) tss
        return (c{instances = is})                

kcConstr :: KcEnv -> Class -> IO (Class,KcEnv)
kcConstr e c
    = do 
        (r,_) <- runKc e (kcBind c)
        e' <- either error (\x -> return x) r
        return (c,e')

kcBind :: Class -> Kc KcEnv
kcBind c@(Class n ts ss as is) 
    = do
        let f (_:>: t) = t
            vs = tv c            
        ks <- mapM (const freshVar) vs
        k <- freshVar
        let venv = foldr (uncurry insertTyVarKind) emptyEnv (zip vs' ks)
            vs' = map (\(TyVar n) -> toId n) vs
            g s = s{env = insertClassKind (toId n) k (unionl (env s) venv)}
            g' s = s{env = (env s) `Map.difference` venv }
        block g g' (mapM_ (kcTy . f) as >> kcContext ss)            
        e <- getKcEnv
        s <- getSubst
        defaulting e                        
    
-- selecting the assumptions that can be used for generate constraints

select :: LabelEnv -> [Assumption] -> [Assumption]
select env as 
    = filter (not . condition) as
      where
         condition (i :>: _)
            = Map.member i env || isDataCon i
         isDataCon = isUpper . head . show    
         
-- grouping assumptions based on its id's, an overloaded identifier
-- is the one that have more than one definition in context.

groupById :: [Assumption] -> [[Assumption]]
groupById 
    = filter moreThanOne . table
      where
         table = Map.elems . foldr ins Map.empty 
         ins a@(i :>: _) ac 
             = case Map.lookup i ac of
                    Just as -> Map.insert i (a : as) ac
                    Nothing -> Map.insert i [a] ac

-- some auxiliar functions

sig2Assump (TypeSig _ ns t) 
    = map (\n -> toId n :>: (f t)) ns
      where
         f x@(TyForall _ _ _) = x
         f y = TyForall Nothing [] y

idof (ClassA n _) = toId n
idof (InfixA _ n _) = toId n    

moreThanOne (_:_:_) = True
moreThanOne _       = False

sameId (i :>: _) (i' :>: _) = i == i'

isBind (FunBind _) = True
isBind (PatBind _ _ _ _ _) = True
isBind _ = False

isTypeSig (TypeSig _ _ _) = True
isTypeSig _ = False       
