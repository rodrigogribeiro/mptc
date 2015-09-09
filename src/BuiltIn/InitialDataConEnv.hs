
module BuiltIn.InitialDataConEnv (initialVarEnv) where


import Language.Haskell.Exts

import Tc.Assumption

import Utils.Id

-- this module defines the initial 
-- data constructor environment. It
-- is specified according with
-- "A static semantics for Haskell"
-- by Faxen.

initialVarEnv :: [Assumption]
initialVarEnv 
    = tuplecons ++ listcons
      

         
listcons :: [Assumption]
listcons 
    = [list, cons, unit]
      where
        list = lid :>: TyForall Nothing [] (TyList v)
        lid = toId (Special ListCon)
        cons = cid :>: TyForall Nothing [] (TyFun v (TyFun (TyList v) (TyList v)))
        cid = toId (Special Cons)
        [v] = take 1 allVars
        unit = uid :>: TyForall Nothing [] (TyCon unitc)
        unitc = Special UnitCon
        uid = toId unitc

tuplecons :: [Assumption]
tuplecons 
    = map f [2..7]
      where
        f x = toId (s x) :>: t x
        s x = Special (TupleCon Boxed x)
        t x = TyForall Nothing [] (tuple x)
        tuple x = let ts = take x allVars
                    in TyFun (foldr1 TyFun ts) (TyTuple Boxed ts)
        
allVars :: [Type]
allVars = [f [x] | x <- ['a'..'z']] ++ 
             [f (x : show i) | i <- [1..], x <- ['a'..'z']]
               where
                  f x = TyVar (Ident x)        
