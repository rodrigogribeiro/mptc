
module Iface.ClassInstCollector(collectClassInstData) where


-- this module does the job of collecting
-- class / instance constraints and to lift
-- class / instance methods.

import Control.Monad.Trans
import Control.Monad (unless, liftM)

import Data.List(partition)
import Data.Maybe (isNothing, fromJust)
import qualified Data.Map as Map

import Language.Haskell.Exts hiding (name)

import Tc.Assumption
import Tc.Class
import Tc.TcEnv
import Tc.TcMonad
import Tc.TcSubst

import Utils.ErrMsg
import Utils.Env
import Utils.Id
import qualified Utils.Nameable as N


-- the main function of the module
-- from a module it returns a set of classes
-- and the set of binding declared in classes
-- and instances defined in the module being compiled

collectClassInstData :: ClassEnv -> Module -> TcM ([Class],[Inst],[Decl])
collectClassInstData env (Module _ _ _ _ _ _ ds)
    = do
        let
            cs = filter isClass ds
            is = filter isInst ds
        (cs', is') <- mkClasses env cs is
        let env' = foldr ins env cs'
            ins c = insert (name c) c
        bs <- liftBinds env' cs is
        return (cs', is', bs)
        
-- creating a new class and instances constraints      
      
mkClasses :: ClassEnv -> [Decl] -> [Decl] -> TcM ([Class], [Inst])
mkClasses env cs is 
    = do
        unless (null is') (undefinedClassError is')
        let cs' = map mkClass cs
            ins c = Map.insert (name c) c
        is' <- genSuperConstraints (foldr ins env cs') (ism ++ isi)
        return (cs', is')
      where
         f (x, y) = (map mkInst x, map mkInst y)
         partition' g = f . partition g
         (ism, isi) = partition' (isDef cs) is
         bad_insts = filter (isNothing . flip lookupEnv env . instname) isi
         is' = map (unid . instname) bad_insts

-- here we generate extra super class constraints for each instance
-- axiom. This will be used by the SAT and context-reduction algorithm

genSuperConstraints :: ClassEnv -> [Inst] -> TcM [Inst] 
genSuperConstraints cs is 
    = do
        is' <- mapM freshInst is
        is'' <- mapM gen is'
        mapM quantifyInst is''
      where
         gen i 
            = do
                c <- maybe (undefinedClassError [unid $ instname i]) 
                           return 
                           (Map.lookup (instname i) cs)
                c' <- freshInst c
                s <- match (parameters c')
                           (instparameters i)
                let ps = apply s (supers c')
                return (i{instsupers = ps ++ instsupers i})                      
                  
         
-- lifting methods from class and instances. 
-- Associates with each its corresponding 
-- type signature.         

liftBinds :: ClassEnv -> [Decl] -> [Decl] -> TcM [Decl]
liftBinds env cs is 
    = do
        bsc <- liftM concat (mapM liftBindsFromClass cs)
        bsi <- liftM concat (mapM (liftBindsFromInst env) is)
        return (bsc ++ bsi)    
        
liftBindsFromClass :: Decl -> TcM [Decl]
liftBindsFromClass (ClassDecl _ ctx n vs _ ds)  
    = do
        let
            c = ClassA (UnQual n) (map unkindvar vs)
            (sigs,bnds) = partition isTypeSig ds
            sigs' = concatMap (insConstr c . unCls) sigs
        return (sigs' ++ map unCls bnds)
        
liftBindsFromInst :: ClassEnv -> Decl -> TcM [Decl]
liftBindsFromInst env (InstDecl _ ctx qn ts ds)
    = do
        let
            i = toId qn
        c <- maybe (undefinedClassError [qn]) freshInst (lookupEnv i env)    
        let 
            s = zip (parameters c) ts
            as' = apply s (members c)
        return ({-- map assump2TypeSig as' ++ --} map unInst ds)
        
assump2TypeSig :: Assumption -> Decl
assump2TypeSig (i :>: t) 
    = TypeSig loc [n] t
      where
        n = unqual (unid i) 
        loc = SrcLoc "" 0 0
                     

-- creating a class from a declaration. 
-- Its instance list is empty.

mkClass :: Decl -> Class
mkClass (ClassDecl _ ctx n vs _ ds) 
    = Class (toId n) vs' ctx sigs []
      where
         vs' = map unkindvar vs
         sigs = map sig2Assump (foldr k [] ss)
         ss = map unCls (filter isTypeSig ds)
         k (TypeSig l ns t) ac 
            = map (\n -> TypeSig l [n] t) ns ++ ac 
            
-- inserting a constraint in a type signature

insConstr :: Asst -> Decl -> [Decl]
insConstr c (TypeSig l ns t) 
    = map (\n -> TypeSig l [n] t') ns
      where
        t' = insC c t
        insC c (TyForall Nothing ctx t)
            = TyForall Nothing (c:ctx) t
        insC c t = TyForall Nothing [c] t            

-- creating a instance from a declaration.

mkInst :: Decl -> Inst
mkInst (InstDecl _ ctx n ts _) 
    = Inst (toId n) ts ctx 
    
-- from signatures to assumptions

sig2Assump :: Decl -> Assumption
sig2Assump (TypeSig _ [n] t)
    = toId n :>: sch t    

-- some auxiliar functions

sch t@TyForall{} = t
sch t = TyForall Nothing [] t
      
isClass :: Decl -> Bool
isClass ClassDecl{} = True
isClass _           = False

isInst :: Decl -> Bool
isInst InstDecl{} = True
isInst _          = False 

isTypeSig :: ClassDecl -> Bool
isTypeSig (ClsDecl TypeSig{}) = True
isTypeSig _                   = False

isDef :: [Decl] -> Decl -> Bool
isDef ds d = any ((N.name d ==) . N.name) ds    

-- extract a declaration from a class decl constructor

unCls (ClsDecl d) = d
unCls x = unsupportedDeclMsg x

unInst (InsDecl d) = d
unInst x = unsupportedDeclMsg x  
  

unkindvar (UnkindedVar v) = TyVar v
unkindvar x = unsupportedKindErrMsg x 
        
