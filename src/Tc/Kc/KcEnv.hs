
module Tc.Kc.KcEnv where

import qualified Data.Map as Map
import Language.Haskell.Exts

import Utils.Id

-- this module defines the kind environment.
-- implementing operations described in 
-- "A Static Semantics for Haskell" by Karl-Filp Fáxen.

-- this type defines what is the identifier

data IdTy = IdClass  -- the id is a type class
          | IdType   -- the id is a type constructor
          | IdVar    -- the id is a type variable
          deriving (Eq, Ord, Show)
          
-- just a wrapper for representing keys in env          
          
data KcEntry = KcEntry {identry :: Id, idty :: IdTy}
               deriving (Eq, Ord, Show)
               
-- the definition of the environment

type KcEnv = Map.Map KcEntry Kind      


-- environment operations

emptyEnv :: KcEnv
emptyEnv = Map.empty

-- inserting a id with it's kind

insertClassKind :: Id -> Kind -> KcEnv -> KcEnv
insertClassKind i k env = insert' IdClass i k env

insertTypeKind :: Id -> Kind -> KcEnv -> KcEnv
insertTypeKind i k env = insert' IdType i k env

insertTyVarKind :: Id -> Kind -> KcEnv -> KcEnv
insertTyVarKind i k env = insert' IdVar i k env

getTyVarsInEnv :: KcEnv -> [(Id,Kind)]
getTyVarsInEnv env 
    = Map.foldrWithKey (\e k ac -> if isVar k then (identry e, k): ac else ac) [] env
      where
         isVar (KindVar _) = True
         isVar _ = False
                     

-- lookingup a id

lookupClassKind :: Id -> KcEnv -> Maybe Kind
lookupClassKind i env = lookup' IdClass i env

lookupTypeKind :: Id -> KcEnv -> Maybe Kind
lookupTypeKind i env = lookup' IdType i env

lookupTyVarKind :: Id -> KcEnv -> Maybe Kind
lookupTyVarKind i env = lookup' IdVar i env

-- getting the domain

domain :: KcEnv -> [Id]
domain = map identry . Map.keys

-- getting the image

image :: KcEnv -> [Kind]
image = Map.elems         

-- removing some entries with specified Id's

(^-) :: KcEnv -> [Id] -> KcEnv
env ^- ids = Map.filterWithKey (\k _ -> (identry k) `notElem` ids) env 

-- projection on a set of ids

(|:) :: KcEnv -> [Id] -> KcEnv
env |: ids = Map.filterWithKey (\n _ -> (identry n) `elem` ids) env

-- unionl preserves entries in left

unionl :: KcEnv -> KcEnv -> KcEnv
unionl env1 env2 = Map.union env1 env2

-- unionr preserves entries in right

unionr :: KcEnv -> KcEnv -> KcEnv
unionr env1 env2 = Map.union env2 env1

-- filtering a environment based on the type of Id

unquals :: KcEnv -> KcEnv
unquals = Map.filterWithKey (\k _ -> isunqual (identry k))

quals :: KcEnv -> KcEnv
quals = Map.filterWithKey (\k _ -> isqual (identry k))

-- some auxiliar functions

lookup' t i env = Map.lookup (KcEntry i t) env 

insert' t i k env = Map.insert (KcEntry i t) k env         
