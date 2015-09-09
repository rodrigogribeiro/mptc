
module Tc.TcInst where

import qualified Data.Map as Map
import Language.Haskell.Exts

import Tc.Class
import Tc.Assumption

import Utils.Debug

-- this module implements the type instantiation
-- algorithm.

type Var = Type -- INVARIANT: Always a type variable

type InstMap = Map.Map Var Type

-- a type class for the instantiation process

class Instantiate t where
    inst :: InstMap -> t -> t
    
instance Instantiate t => Instantiate [t] where
    inst m = map (inst m)
    
instance Instantiate Asst where
    inst m (ClassA n ts) = ClassA n (inst m ts)
    inst m (InfixA l n r) = InfixA (inst m l) n (inst m r)    
    
instance Instantiate Inst where
    inst m (Inst n ts ps) = Inst n (inst m ts) (inst m ps)    
    
instance Instantiate Class where
    inst m (Class n ts ss ms is) 
        = Class n (inst m ts) (inst m ss) (inst m ms) (inst m is)    
    
instance Instantiate Type where
    inst m (TyForall x ctx t) 
        = TyForall x (inst m ctx) (inst m t)
    inst m (TyFun l r)
        = TyFun (inst m l) (inst m r)
    inst m (TyTuple b ts)
        = TyTuple b (inst m ts)
    inst m (TyList t)
        = TyList (inst m t)
    inst m (TyParen t)
        = TyParen (inst m t)
    inst m (TyInfix l n r)
        = TyInfix (inst m l) n (inst m r)
    inst m v@(TyVar _) 
        = case Map.lookup v m of
                Just t -> t
                Nothing -> v
    inst m (TyApp l r) 
        = TyApp (inst m l) (inst m r)                
    inst m t = t         
    
instance Instantiate Assumption where
    inst m (i :>: t) = i :>: (inst m t)       
    
instantiate :: (Instantiate t) => [Var] -> [Type] -> t -> t
instantiate vs ts t = inst tymap t
                      where
                        tymap = Map.fromList (zip vs ts)                         