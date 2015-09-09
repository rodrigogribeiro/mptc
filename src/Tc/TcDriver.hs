
module Tc.TcDriver (tcDriver) where

-- this module contains the code that starts all 
-- type inference / checking algorithm.

import Data.List((\\))
import Language.Haskell.Exts hiding (name)

import Iface.Iface hiding (instances)
import Iface.DataConsCollector
import Iface.ClassInstCollector
import Iface.InstDeriving

import Tc.Kc.KcEnv hiding (image)
import Tc.Assumption
import Tc.TcAlphaEq
import Tc.Class
import Tc.TcClassInstGen
import Tc.TcDecl
import Tc.TcMonad
import Tc.TcEnv
import Tc.TySyn
import Tc.TcLabel
import Tc.TcSubst
import Tc.TcCriteria (toAsst, anyM, className)

import Utils.EnvMonad
import Utils.Env
import Utils.Stack hiding (fromList)
import Utils.ErrMsg hiding (empty)
import Utils.Id
import qualified Utils.Nameable as N


-- this is the main function of this module.
-- It receives the kind environment of current module,
-- the imported classes, instances and assumptions
-- loaded from interface files.

tcDriver :: [TySyn] ->       -- type synonyms loaded from the interfaces
            LabelEnv ->      -- labels loaded from the interfaces
            KcEnv ->         -- kinds loaded from the interfaces
            [Class] ->       -- classes loaded from the interfaces
            [Inst] ->        -- instances loaded from the interfaces
            [Assumption] ->  -- assumptions loaded from the interfaces
            Module -> IO Iface
tcDriver syns lbls kcenv cls ins assumps m 
    = do
        (r,_) <- run genEnv (buildEnv syns kcenv lbls cls ins assumps m)
        either error (uncurry4 (tcMain m)) r
        

-- this function starts the inference algorithm
        
tcMain :: Module -> TcEnv -> [Decl] -> [Assumption] -> Iface -> IO Iface
tcMain (Module _ _ _ _ _ _ ds) e binds as i
    = do
        let 
          ids = map (\(i :>: _) -> i) as
          cond d = isBind d && (N.name d `notElem` ids) -- removing the overloaded bindings
          binds' = (filter cond ds) \\ binds
        (r, _) <- run e (tcDecls binds' binds)      
        either error (\x -> return i{assumps = (assumps i) ++ x}) r         

-- Building an initial type inference environment
-- !!!!!!pay attention to this function.!!!!
-- it does a lot of work related to creating constraints for 
-- overloaded bindings

    
buildEnv :: [TySyn] -> KcEnv -> LabelEnv -> [Class] -> 
            [Inst] -> [Assumption] -> Module -> TcM (TcEnv, [Decl], [Assumption], Iface)
buildEnv syns kc lenv cls inst ass m 
    = do
    	-- collecting data constructors from the current module
        (cons,projs,lbls) <- collectDataConsInfo m
        let clsenv1 = mkClassEnv empty cls inst
            lenv' = foldr (uncurry insert) lenv lbls
        -- collecting class/instances definitions from the current module
        (cls', inst', binds) <- collectClassInstData clsenv1 m
        -- generating class instance constraints from binding definitions
        let clsenv2 = mkClassEnv clsenv1 cls' inst'
        (ocls,obs,kc',osigs) <- genClassInsts clsenv2 kc lenv ass m
        let sig = map sig2Assump (filter isTypeSig binds) 
            sig' = (filter (flip notElem osigs) sig) ++ mems 
            mems = concatMap members ocls
        -- generate instance constraints based on deriving clauses
        dins <- deriveInsts clsenv2 m
        let clsenvf = insertIns clsenv2' dins
            clsenv2' = foldr (\c ac -> insert (name c) c ac) clsenv2 ocls 
            lenv1 = foldr (uncurry insert) empty lbls
            inst'' = inst' ++ (concatMap instances ocls)
            iface = Iface (moduleName m) syns lenv1 kc' (cls' ++ ocls) inst'' (cons ++ mems)
            e = mkTcEnv clsenvf (sig' ++ cons ++ projs ++ ass) lenv'
        -- check for obvious invalid instances
        checkInstances clsenvf (dins ++ inst')
        return (e, (binds ++ obs), osigs, iface)  
        
-- Building a class env from a list of classes and instances
-- loaded from the interfaces files.

mkClassEnv :: ClassEnv -> [Class] -> [Inst] -> ClassEnv
mkClassEnv env cls ins 
    = foldr (\c ac -> insert (name c) c ac) env cls'
      where
         cls' = foldr go [] cls
         go c ac = c{instances = 
                [i | i <- ins, 
                    (instname i) == (name c)]} : ac


insertIns :: ClassEnv -> [Inst] -> ClassEnv
insertIns clsenv is 
    = mapEnv ins clsenv
      where
         ins c = c{instances = is' ++ (instances c)}
                 where
                    is' = [i | i <- is, (instname i) == (name c)]

-- checking instance declarations

checkInstances :: ClassEnv -> [Inst] -> TcM ()
checkInstances env ins 
    = do
        -- first step: instantiate instance's types
        ins' <- mapM freshInst ins
        -- second: getting constraints to be checked
        need_check <- liftM (zip ins') (mapM (classMatch env) ins')
        -- third: filtering unsolved instances
        bad_insts <- filterM (liftM not . check env) need_check
        unless (null bad_insts) (undefinedInstanceError bad_insts)
        return ()
        
classMatch :: ClassEnv -> Inst -> TcM [Asst]
classMatch env i 
    = do
        let n = instname i
            classToAsst c = ClassA (unid (name c)) (parameters c)            
        c <- maybe (classNotDefinedError $ unid n) freshInst (lookupEnv n env)
        s <- match (classToAsst c) (toAsst i)
        let solved = filter (null . fv) $ apply s (supers c) 
        return solved        
        
check :: ClassEnv -> (Inst, [Asst]) -> TcM Bool
check _ (_, []) = return True
check env (i, ctx) 
    = do
       anyM check' ctx
      where
        check' c 
            = do 
                let n = className c
                is <- maybe (classNotDefinedError n) 
                            (return . instances)  
                            (lookupEnv (toId n) env)  
                anyM (alphaEqM c) (map toAsst is)

-- some auxiliar functions          

sameId (i :>: _) (i' :>: _) = i == i'    
        
uncurry4 :: (t1 -> t2 -> t3 -> t4 -> t) -> (t1, t2, t3, t4) -> t
uncurry4 f (x,y,z,k) = f x y z k

gamtest :: Stack (Env a)
gamtest = singleton $ fromList []
clstest = empty

genEnv = TcEnv clstest gamtest empty emptyStack 0 []      

isBind (FunBind _) = True
isBind (PatBind _ _ _ _ _) = True
isBind (TypeSig _ _ _) = True
isBind _ = False  

moduleName (Module _ (ModuleName s) _ _ _ _ _) = toId (Ident s)
