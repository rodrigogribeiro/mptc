
module Utils.ExpandTySyn where

import Data.Generics(mkM,everywhereM)
import Data.List (union, partition)
import Data.Maybe (mapMaybe)
import Language.Haskell.Exts

import Tc.TySyn

import Utils.EnvMonad
import Utils.ErrMsg

-- this module does the type synonym expansion algorithm
                          
-- substitution. INVARIANT: left component is ALWAYS a variable.

type Subst = [(Type,Type)]                          
                          
-- the monad for type synonym expansion

type TySynM a = EnvM [TySyn] a

-- expanding synonyms in syntax tree.
-- First parameter: Synonyms defined in another modules.

expandSynsInModule :: [TySyn] -> Module -> IO ([TySyn], Module)
expandSynsInModule syns (Module loc n x y exps imps decls) 
    = do
        let
           decls' = concatMap breakSig decls
           (s,ds) = partition isTySyn decls'
           syns' = map toTySyn s
           e = syns ++ syns'
        (r,_) <- run e (everywhereM (mkM expand) ds)
        return $ either error (\s' -> (syns', Module loc n x y exps imps (s ++ s'))) r
        
-- breaking type signatures

breakSig :: Decl -> [Decl]
breakSig (TypeSig l ns t) = map (\n -> TypeSig l [n] t) ns
breakSig d = [d]               

-- expanding all synonyms in one type

expand :: Type -> TySynM Type
expand t = do
            ty <- lookupTySyn t
            case ty of
                Just t' -> everywhereM (mkM expand) t'
                Nothing -> return t
                
lookupTySyn :: Type -> TySynM (Maybe Type)
lookupTySyn ty = gets (findTy ty)
                 where
                    findTy ty syns 
                        = case mapMaybe (matchSyn ty) syns of
                                [] -> Nothing
                                (t:_) -> Just t
                    matchSyn ty (l, r) 
                        = case match ty l of
                                Nothing -> Nothing
                                Just s  -> Just (apply s r)                                 


-- a class for applying a substitution

class Apply a where
    apply :: Subst -> a -> a
    
instance Apply a => Apply [a] where
    apply s = map (apply s)    

instance Apply Type where
    apply s (TyForall m ctx ty)
        = TyForall m (apply s ctx) (apply s ty)
    apply s (TyFun l r) 
        = TyFun (apply s l) (apply s r)
    apply s (TyTuple x ts)
        = TyTuple x (apply s ts)
    apply s (TyList t) 
        = TyList (apply s t)
    apply s (TyApp l r)
        = TyApp (apply s l)
                (apply s r)
    apply s (TyParen t)
        = TyParen (apply s t)
    apply s (TyInfix l qn r) = TyInfix (apply s l) qn 
                                       (apply s r)   
    apply s v@(TyVar _) = case lookup v s of
                                Nothing -> v
                                Just t  -> t
    apply s t = t

instance Apply Asst where
    apply s (ClassA n ts) = ClassA n (apply s ts)
    apply s (InfixA l n r) = InfixA (apply s l) n (apply s r)
    apply s x = unsupportedDeclMsg x                                                                                                                

-- a matching operation is necessary for checking if a synonym is applyable.

match :: Type -> Type -> Maybe Subst
match  (TyForall _ _ ty1) (TyForall _ _ ty2)
    = match ty1 ty2
match (TyFun l1 r1) (TyFun l2 r2)
    = do
        s1 <- match l1 l2
        s2 <- match (apply s1 r1)
                    (apply s1 r2)
        return (s1 `union` s2)    
match (TyTuple _ tys1) (TyTuple _ tys2)
    | length tys1 == length tys2 
        = do 
            s <- zipWithM match tys1 tys2
            return (foldr union [] s)
    | otherwise = Nothing
match (TyList t1) (TyList t2) = match t1 t2    
match (TyApp l1 r1) (TyApp l2 r2)
    = do
        s1 <- match l1 l2
        s2 <- match (apply s1 r1)
                    (apply s1 r2)
        return (s1 `union` s2)
match (TyParen t1) (TyParen t2) = match t1 t2        
match (TyCon n1) (TyCon n2)
    | n1 == n2 = return []
    | otherwise = Nothing
match (TyInfix l1 n1 r1) (TyInfix l2 n2 r2) = match (TyApp (TyApp (TyCon n1) l1) r1)
                                                    (TyApp (TyApp (TyCon n2) l2) r2)    
match t v@(TyVar _) = return [(v,t)]
match _ _ = Nothing            


-- some auxiliar functions

toTySyn :: Decl -> TySyn
toTySyn (TypeDecl _ n vs ty) 
    = (ty', ty)
      where
        ty' = foldl TyApp (TyCon (UnQual n)) (map f vs)
        f (KindedVar n _) = TyVar n
        f (UnkindedVar x) = TyVar x 


isTySyn :: Decl -> Bool
isTySyn (TypeDecl _ _ _ _) = True
isTySyn _ = False          
         
     