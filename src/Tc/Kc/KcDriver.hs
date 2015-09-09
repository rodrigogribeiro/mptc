
module Tc.Kc.KcDriver where

-- this module contains the main functions of
-- the kind inference process

import Control.Monad(foldM, liftM, zipWithM_)
import Data.Generics (listify)
import Data.Maybe(fromJust)
import qualified Data.Map as Map (foldrWithKey, insert, difference)
import Language.Haskell.Exts hiding (name)

import Tc.Kc.KcEnv
import Tc.Kc.KcMonad
import Tc.Kc.KcSubst

import Utils.DependencyAnalysis
import Utils.EnvMonad(block)
import Utils.ErrMsg
import Utils.Id
import Utils.Nameable
import Control.Monad.Trans (liftIO)

kcDriver :: Module -> KcEnv -> IO KcEnv
kcDriver (Module _ _ _ _ _ imps decls) env
    = do
        let 
            decls' = select decls
            sigs   = filter isSig decls
        (r,_) <- runKc env (kc decls' sigs)
        case r of
            Left err -> error err
            Right env'  -> return env' 
        
-- the kind inferece / checking algorithm

kc :: [Decl] -> [Decl] -> Kc KcEnv       
kc ds sigs 
    = do
         let 
           dss = dependencyAnalysis KindAnalysis ds
           ns = map (map name) dss
         env' <- foldM (\ac ds -> liftM (unionl ac) (kcGroup ds)) emptyEnv dss
         let 
            g s = s{env = unionl (env s) env'}
            g' s = s{env = (env s) `Map.difference` env'}
         -- checking kinds in binds signatures            
         block g g' (kcSigs sigs)
         return env'

-- checking a group of mutually recursive declarations

kcGroup :: [Decl] -> Kc KcEnv
kcGroup ds = do
               ks <- mapM (const freshVar) ds
               mapM_ (uncurry insKind) (zip ds ks)
               env <- foldM (\ac d -> liftM (unionr ac) (kcDecl d)) emptyEnv ds
               defaulting env
               
insKind :: Decl -> Kind -> Kc ()
insKind d@(ClassDecl _ _ _ _ _ _) k = addClassKind (name d) k
insKind d k = addTypeKind (name d) k              

-- checking a declaration
               
kcDecl :: Decl -> Kc KcEnv
kcDecl d@(ClassDecl _ _ _ _ _ _) = kcClass d
kcDecl d@(TypeDecl _ _ _ _) = kcType d
kcDecl d@(DataDecl _ _ _ _ _ _ _) = kcData d

-- checking a type class

kcClass :: Decl -> Kc KcEnv
kcClass c@(ClassDecl _ ctx n tvs _ ds) 
    = withDiagnostic c $ do
        ktvs <- mapM (const freshVar) tvs
        let 
           venv = foldr f emptyEnv (zip tvs ktvs)
           f (v,k) ac = insertTyVarKind (toId v) k ac
           k = foldr1 KindFn ktvs
           g s = s{env = insertClassKind (toId n) k (unionl (env s) venv)}
           g' s = s{env = (env s) `Map.difference` venv }
           sigs = filter isSig $ (map extractDecl) ds
        block g g' (kcContext ctx >> kcSigs sigs)
        s <- getSubst
        return (insertClassKind (toId n) (apply s k) emptyEnv)

-- checking a type synonym        
        
kcType :: Decl -> Kc KcEnv
kcType t@(TypeDecl _ n tvs ty) 
    = withDiagnostic t $ 
     do
        ktvs <- mapM (const freshVar) tvs
        kty <- freshVar
        let
           i = toId n
           venv = foldr f emptyEnv (zip tvs ktvs)
           f (v,k) ac = insertTyVarKind (toId v) k ac
           k = foldr KindFn kty ktvs
           g s = s {env = insertTypeKind i k (unionl (env s) venv)}
           g' s = s {env = (env s) `Map.difference` venv}
        kty' <- block g g' (kcTy ty)
        unify kty kty'
        s <- getSubst
        return (insertTypeKind i (apply s k) emptyEnv)  

-- checking a algebraic data type declaration

kcData :: Decl -> Kc KcEnv
kcData d@(DataDecl _ _ ctx n tvs cons _) 
    = withDiagnostic d $ 
      do
        ktvs <- mapM (const freshVar) tvs
        let
          i = toId n
          k = foldr KindFn KindStar ktvs
          venv = foldr f emptyEnv (zip tvs ktvs)
          f (v,k) ac = insertTyVarKind (toId v) k ac
          g s = s{env = insertTypeKind i k (unionl (env s) venv)}
          g' s = s{env = (env s) `Map.difference` venv}
        block g g' (kcContext ctx >> kcDataCons cons)
        s <- getSubst
        return (insertTypeKind i (apply s k) emptyEnv)
       
-- checking a context       
        
kcContext :: Context -> Kc ()
kcContext = mapM_ kcAsst
            
kcAsst :: Asst -> Kc ()
kcAsst (InfixA l qn r) 
       = kcAsst (ClassA qn [l,r])                                        
kcAsst c@(ClassA qn ts) 
       = do
           -- it could be dangerous... I don't know if it's safe.
           m <- getClassKind (toId qn) 
           ks' <- mapM kcTy ts
           maybe (classNotDefinedError qn) (unify (foldr1 KindFn ks')) m

-- checking data constructors

kcDataCons :: [QualConDecl] -> Kc ()
kcDataCons = mapM_ kcDataCon



kcDataCon :: QualConDecl -> Kc ()
kcDataCon (QualConDecl _ _ _ cd) 
    = kcCon cd
      where
        kcCon (RecDecl n fs) 
            = kcCon (ConDecl n (map snd fs)) 
        kcCon (InfixConDecl l n r) 
            = kcCon (ConDecl n [l,r])
        kcCon (ConDecl n bts) 
            = do 
                ks <- mapM (kcTy . unbang) bts
                mapM_ (unify KindStar) ks 

-- checking a type

kcTy :: Type -> Kc Kind
kcTy (TyForall Nothing ctx ty) 
    = do
        kcContext ctx
        kcTy ty
kcTy (TyForall (Just tvs) ctx ty)
    = do
        ktvs <- mapM (const freshVar) tvs
        let
           venv = foldr f emptyEnv (zip tvs ktvs)
           f (v,k) ac = insertTyVarKind (toId v) k ac                
           g s = s{env = unionl (env s) venv}
           g' s = s{env = (env s) `Map.difference` venv}
        block g g' (kcContext ctx >> kcTy ty)
kcTy (TyFun l r)
    = do
        kl <- kcTy l
        kr <- kcTy r
        unify kl KindStar
        unify kr KindStar
        return KindStar
kcTy (TyTuple _ tys) 
    = do
        kvs  <- mapM (const freshVar) tys
        kvs' <- mapM kcTy tys
        zipWithM_ unify kvs kvs'
        return KindStar          
kcTy (TyList ty)
    = do 
        k <- kcTy ty
        unify k KindStar
        return KindStar
kcTy (TyApp l r) 
    = do
        kl <- kcTy l
        kr <- kcTy r
        v <- freshVar
        unify (KindFn kr v) kl
        s <- getSubst
        return (apply s v)        
kcTy (TyVar n)
    = getTyVarKind (toId n) >>= maybe (kindNotFoundError n) return     
kcTy (TyCon qn)
    = getTypeKind (toId qn) >>= maybe (kindNotFoundError qn) return 
kcTy (TyParen ty) = kcTy ty
kcTy (TyInfix l qn r) = kcTy (TyApp (TyApp (TyCon qn) l) r)
kcTy x = unsupportedDeclMsg x

fromJust1 x Nothing = error (show x)
fromJust1 _ (Just x) = x

-- checking signatures                       
        
kcSigs :: [Decl] -> Kc ()
kcSigs = mapM_ kcSig


kcSig :: Decl -> Kc ()
kcSig s@(TypeSig _ _ ty) 
    = withDiagnostic s $ do
        let
          vs = tyvars ty
          g x s = s{env = (env s) `unionl` x}
        kvs <- mapM (const freshVar) vs
        vs' <- getTyVarsKind
        let
          vk = map fst vs'
          vs'' = filter (\(i,_) -> i `notElem` vk) (zip vs kvs)
          m = foldr (\(i,k) ac -> insertTyVarKind i k ac) emptyEnv vs''
          g' s = s {env = (env s) `Map.difference` m}
        block (g m) g' (kcTy ty >> return ())                
               
-- this function does the kind defaulting over a kind group

defaulting :: KcEnv -> Kc KcEnv
defaulting env = getSubst >>= \s -> 
                   return $ Map.foldrWithKey 
                      (\v k ac -> Map.insert v (defaultKind (apply s k)) ac) 
                         emptyEnv env  

defaultKind :: Kind -> Kind
defaultKind KindStar = KindStar
defaultKind (KindFn l r) = KindFn (defaultKind l) 
                                  (defaultKind r)
defaultKind (KindParen k) = KindParen (defaultKind k)
defaultKind (KindVar _) = KindStar                                               
        
-- some auxiliar functions

select :: [Decl] -> [Decl]        
select = filter isKindDecl
         where
            isKindDecl (TypeDecl _ _ _ _) = True
            isKindDecl (DataDecl _ _ _ _ _ _ _) = True
            isKindDecl (ClassDecl _ _ _ _ _ _) = True
            isKindDecl _ = False

isSig :: Decl -> Bool
isSig (TypeSig _ _ _) = True
isSig _ = False

extractDecl :: ClassDecl -> Decl
extractDecl (ClsDecl d) = d
extractDecl x = unsupportedDeclMsg x       

unbang (BangedTy t) = t
unbang (UnBangedTy t) = t
unbang (UnpackedTy t) = t

tyvars t = map f (listify isVar t)
           where
              isVar (TyVar _) = True
              isVar _ = False
              f (TyVar n) = toId n    

       