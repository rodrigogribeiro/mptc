
module BuiltIn.InitialKindEnv (initialKindEnv) where

import Language.Haskell.Exts

import BuiltIn.BuiltInTypes

import Tc.Kc.KcEnv
import Utils.Id

-- this module defines the initial kind
-- enviroment, according the semantics 
-- specified by Fáxen in 
-- "A static semantics for Haskell"

initialKindEnv :: KcEnv
initialKindEnv 
    = env2
      where
         env1 = foldr f emptyEnv (primtypes ++ tupletypes) 
         f t = insertTypeKind (pprint t) KindStar 
         env2 = foldr g env1 others
         g (t,k) = insertTypeKind (idfrom t) k
    
primtypes :: [Type]
primtypes = [tInt, tChar, tDouble, tFloat,
             tDouble, tInteger, tString, 
             tString1, tBool] -- must be stripped out after
                              -- prelude    
             
tupletypes :: [Type]
tupletypes 
    = map f [2..7]
      where
         f x = TyCon (Special (TupleCon Boxed x))
         
others :: [(Type, Kind)]         
others = zip (map (TyCon . Special) [FunCon, ListCon, UnitCon])
             [KindFn KindStar (KindFn KindStar KindStar), KindFn KindStar KindStar, KindStar]
          
pprint :: Type -> Id
pprint = toId . Ident . prettyPrint   

idfrom :: Type -> Id
idfrom (TyCon qn) = toId qn                      
