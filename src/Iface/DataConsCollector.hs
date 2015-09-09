
module Iface.DataConsCollector(collectDataConsInfo) where


-- this module contains functions for collecting
-- data constructors from the current module and
-- transform them in a set of assumption for the
-- the type environment. 
-- It also generates assumptions for record 
-- projection functions

import Control.Arrow (first, second)
import Control.Monad

import Language.Haskell.Exts

import Tc.Assumption
import Tc.TcMonad 
import Tc.TcLabel

import Utils.Id


-- the main function

-- first assumption list: data constructors
-- second assumption list: record projections

collectDataConsInfo :: Module -> TcM ([Assumption], [Assumption], [(Id,[Label])])
collectDataConsInfo (Module _ _ _ _ _ _ ds)
    = do
        let
            ds' = filter isDataDecl ds
            ds'' = filter hasRecConDecl ds'
        as <- liftM concat (mapM dataConsAssump ds')
        (as', l) <- mapAndUnzipM recProject ds''
        return (as, concat as', concat l)
        
-- generating data constructor assumptions

dataConsAssump :: Decl -> TcM [Assumption]
dataConsAssump (DataDecl _ _ ctx n vs cons _)
    = do
        let tc = mkTyCon n vs
        mapM (mkDataConAssump ctx tc) cons    

-- generating record projection assumptions

recProject :: Decl -> TcM ([Assumption], [(Id, [Label])])
recProject (DataDecl _ _ ctx n vs ds _) 
    = do
        let
            uncon (QualConDecl _ _ _ d) = d
            ds' = filter recDecl (map uncon ds)
            tc = mkTyCon n vs
        (assp, rc) <- mapAndUnzipM (recProj tc) ds'
        return (concat assp, rc)  
        
recProj tc (RecDecl n fs) 
    = return (foldr step [] fs', fs'')
      where
         fs' = foldr go [] fs
         fs'' = (toId n, map (first toId) fs')
         go (ns,t) ac 
            = map (\n -> (n, unbang t)) ns ++ ac
         step (n,t) ac 
            = (i' :>: TyForall Nothing [] t1) : ac
              where
                 i' = toId n
                 t1 = TyFun tc t       
         
-- selecting data type declarations

isDataDecl :: Decl -> Bool
isDataDecl DataDecl{} = True
isDataDecl _          = False         

-- selecting data type declarations that has record definitions

hasRecConDecl :: Decl -> Bool
hasRecConDecl (DataDecl _ _ _ _ _ ds _)
    = any recDecl ds'
      where
        ds' = map (\(QualConDecl _ _ _ d) -> d) ds
hasRecConDecl _ = False    

recDecl (RecDecl _ _) = True
recDecl _ = False

-- generating a type constructor

mkTyCon :: Name -> [TyVarBind] -> Type
mkTyCon n vs 
    = foldl TyApp (TyCon (UnQual n)) vs'
      where
        vs' = map f vs
        f (UnkindedVar n) = TyVar n
        
-- generating a data constructor assumption

mkDataConAssump :: Context -> Type -> QualConDecl -> TcM Assumption
mkDataConAssump ctx t (QualConDecl _ _ _ c)
    = return (uncurry (:>:) (second (TyForall Nothing ctx) (nameType t c)))
    
nameType :: Type -> ConDecl -> (Id, Type)
nameType t (ConDecl n ts) 
    = (toId (UnQual n), foldr (TyFun . unbang) t ts) 
nameType t (InfixConDecl l n r) 
    = (toId (UnQual n), foldr (TyFun . unbang) t [l,r])
nameType t (RecDecl n fs) 
    = (toId (UnQual n), foldr (TyFun . unbang) t ts)
      where
        ts = concat (foldr f [] fs)
        f (ns, t') ac = replicate (length ns) t' : ac          

-- some auxiliar functions

unbang (BangedTy t) =  unparen t
unbang (UnBangedTy t) = unparen t
unbang (UnpackedTy t) = unparen t

unparen (TyParen t) = t
unparen x = x
