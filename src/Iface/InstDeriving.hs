
module Iface.InstDeriving(deriveInsts) where


-- this module implements a function for the
-- instance deriving mechanism. It does not
-- generate functions, it only creates constraints
-- for the class environment.
-- It will throw errors for undefined / not visible 
-- type classes.

import Control.Monad (liftM)
import Language.Haskell.Exts

import Tc.Class
import Tc.TcEnv
import Tc.TcMonad

import Utils.Id
import Utils.Env
import Utils.ErrMsg

deriveInsts :: ClassEnv -> Module -> TcM [Inst]
deriveInsts env (Module _ _ _ _ _ _ ds)
    = deriveInsts' env ds

deriveInsts' :: ClassEnv -> [Decl] -> TcM [Inst]
deriveInsts' env ds 
    = liftM concat (mapM (derive env) (filter isDataDecl ds))
    
derive :: ClassEnv -> Decl -> TcM [Inst]
derive env (DataDecl _ _ ctx n vs _ ds)  
    = mapM (gen env t ctx) ds
      where 
        vs' = map unkindvar vs
        t = foldl TyApp tc vs'
        tc = TyCon (UnQual n)
        
        
gen env t ctx (c,ts) 
    = return (Inst (toId c)
                   (ts ++ [t])
                   ctx)
                           
-- some auxiliar functions

unkindvar (UnkindedVar n) = TyVar n
unkindvar x = unsupportedKindErrMsg x

isDataDecl :: Decl ->  Bool
isDataDecl DataDecl{} = True
isDataDecl _          = False    
