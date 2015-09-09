module Utils.Nameable where

import Data.List (union)
import Data.Generics
import Language.Haskell.Exts hiding (name)

-- a module for some auxiliar functions for dependency
-- analysis

import Utils.Id
import Utils.ErrMsg

-- a type class for some operations need by dependency
-- analysis

class Show a => Nameable a where
   -- returns the declaration name
   name :: a -> Id
   
-- a instance for the declaration AST

instance Nameable Decl where
   name (TypeDecl _ n _ _)       = toId n
   name (DataDecl _ _ _ n _ _ _) = toId n
   name (ClassDecl _ _ n _ _ _)  = toId n
   name (InstDecl _ _ n _ _)     = toId n
   name (TypeSig _ [n] _)        = toId n
   name (FunBind ms)             = name (head ms)
   name (PatBind _ p _ _ _)      = toId (Ident (prettyPrint p)) 
   name d                        = unsupportedDeclMsg d
   
instance Nameable Match where
   name (Match _ n _ _ _ _) = toId n


-- getting names in a declaration for kind analysis
  
class Referenceable a where
   kindRefNames :: a -> [Id]
   -- INVARIANT: here we must lift default and instance methods
   --            from class / instance definitions, before
   --            starting the dependency analysis algorithm.
   typeRefNames :: a -> [Id]
   
   kindRefNames _ = []
   typeRefNames _ = []
   
instance Referenceable a => Referenceable [a] where
   kindRefNames = foldr (union . kindRefNames) []
   typeRefNames = foldr (union . typeRefNames) []
   
instance Referenceable Decl where
   kindRefNames (TypeDecl _ _ _ ty) = kindRefNames ty
   kindRefNames (DataDecl _ _ ctx _ _ cons _)
        = kindRefNames ctx `union` kindRefNames cons
   kindRefNames (ClassDecl _ ctx _ _ _ decls)
        = kindRefNames ctx `union` kindRefNames decls
   kindRefNames _ = []
   
   typeRefNames (FunBind ms) = typeRefNames ms
   typeRefNames (PatBind _ _ _ rhs bs) = typeRefNames rhs `union` typeRefNames bs
   typeRefNames _ = []
   
instance Referenceable Type where
   kindRefNames (TyForall _ ctx ty) 
        = kindRefNames ctx `union` kindRefNames ty
   kindRefNames (TyFun l r) 
        = kindRefNames l `union` kindRefNames r
   kindRefNames (TyTuple _ ts)
        = kindRefNames ts
   kindRefNames (TyList ty)
        = kindRefNames ty
   kindRefNames (TyApp l r)
        = kindRefNames l `union` kindRefNames r
   kindRefNames (TyCon c)
        = [toId c]
   kindRefNames (TyParen ty) 
        = kindRefNames ty
   kindRefNames (TyInfix l n r) 
        = (toId n) : kindRefNames l `union` kindRefNames r
   kindRefNames _ = []
   
instance Referenceable Asst where
   kindRefNames (ClassA c ts) 
        = (toId c) : kindRefNames ts
   kindRefNames (InfixA l c r)
        = (toId c) : kindRefNames l `union` kindRefNames r
   kindRefNames _ = []
   
instance Referenceable QualConDecl where
   kindRefNames (QualConDecl _ _ ctx c)
        = kindRefNames ctx `union` kindRefNames c
        
instance Referenceable ConDecl where
   kindRefNames (ConDecl _ ts)
        = kindRefNames ts
   kindRefNames (InfixConDecl l _ r)
        = kindRefNames l `union` kindRefNames r
   kindRefNames (RecDecl _ fs)
        = kindRefNames (map snd fs) 
        
instance Referenceable ClassDecl where
   kindRefNames (ClsDecl d)
        = kindRefNames' d
   kindRefNames _ = []      
   
instance Referenceable BangType where
   kindRefNames (BangedTy t) 
        = kindRefNames t
   kindRefNames (UnBangedTy t)
        = kindRefNames t
   kindRefNames (UnpackedTy t)
        = kindRefNames t   

instance Referenceable Binds where
   typeRefNames (BDecls ds) = typeRefNames ds
   typeRefNames _           = []
   
instance Referenceable Match where
   typeRefNames (Match _ _ _ _ rhs bs) = typeRefNames rhs `union`
                                         typeRefNames bs
                                         
instance Referenceable Rhs where
   typeRefNames (UnGuardedRhs e) = typeRefNames e
   typeRefNames (GuardedRhss gs) = typeRefNames gs
   
instance Referenceable GuardedRhs where
   typeRefNames (GuardedRhs _ ss e) = typeRefNames ss `union`
                                      typeRefNames e
                                      
instance Referenceable Stmt where
   typeRefNames (Generator _ _ e) = typeRefNames e
   typeRefNames (Qualifier e) = typeRefNames e
   typeRefNames (LetStmt bs) = typeRefNames bs
   typeRefNames (RecStmt ss) = typeRefNames ss
   
instance Referenceable Exp where
   typeRefNames (Var qn) = [toId qn]
   typeRefNames (Con qn) = [toId qn]
   typeRefNames (Lit _)  = []
   typeRefNames (InfixApp l op r) = toId op : typeRefNames [l,r]
   typeRefNames (App l r) = typeRefNames [l,r]
   typeRefNames (NegApp e) = typeRefNames e
   typeRefNames (Lambda _ _ e) = typeRefNames e
   typeRefNames (Let bs e) = typeRefNames bs `union` typeRefNames e
   typeRefNames (If e1 e2 e3) = typeRefNames [e1, e2, e3]
   typeRefNames (Case e alts) = typeRefNames e `union` typeRefNames alts
   typeRefNames (Do stmts) = typeRefNames stmts
   typeRefNames (Tuple _ es) = typeRefNames es
   typeRefNames (List es) = typeRefNames es
   typeRefNames (Paren e) = typeRefNames e
   typeRefNames (LeftSection e op) = toId op : typeRefNames e
   typeRefNames (RightSection op e) = toId op : typeRefNames e
   typeRefNames (RecConstr qn fs) = toId qn : typeRefNames fs
   typeRefNames (RecUpdate e us) = typeRefNames e `union` typeRefNames us
   typeRefNames (EnumFrom e) = typeRefNames e
   typeRefNames (EnumFromTo e1 e2) = typeRefNames [e1,e2]
   typeRefNames (EnumFromThen e1 e2) = typeRefNames [e1,e2]
   typeRefNames (EnumFromThenTo e1 e2 e3) = typeRefNames [e1,e2,e3]   
   typeRefNames (ListComp e qsmt) = typeRefNames e `union` typeRefNames qsmt
   typeRefNames (ExpTypeSig _ e _) = typeRefNames e
   typeRefNames (CorePragma _ e) = typeRefNames e
   typeRefNames (SCCPragma _ e) = typeRefNames e
   typeRefNames e = unsupportedExpErrMsg e
   
instance Referenceable Alt where
   typeRefNames (Alt _ _ gs bs) = typeRefNames gs `union` typeRefNames bs
   
instance Referenceable GuardedAlts where
   typeRefNames (UnGuardedAlt e) = typeRefNames e
   typeRefNames (GuardedAlts alts) = typeRefNames alts
   
instance Referenceable GuardedAlt where
   typeRefNames (GuardedAlt _ ss e) = typeRefNames ss `union` typeRefNames e
   
instance Referenceable FieldUpdate where
   typeRefNames (FieldUpdate qn e) = toId qn : typeRefNames e
   typeRefNames (FieldPun n) = [toId n]
   typeRefNames FieldWildcard = []
   
instance Referenceable QualStmt where
   typeRefNames (QualStmt s) = typeRefNames s
   typeRefNames (ThenTrans e) = typeRefNames e
   typeRefNames (GroupBy e) = typeRefNames e
   typeRefNames (GroupUsing e) = typeRefNames e
   typeRefNames (GroupByUsing e1 e2) = typeRefNames [e1,e2] 

-- some auxiliar functions
        
kindRefNames' (TypeSig _ _ ty) = kindRefNames ty
kindRefNames' _ = []

typeRefNames' (FunBind ms) = typeRefNames ms
typeRefNames' (PatBind _ _ _ r ds) 
    = typeRefNames r `union` typeRefNames ds
typeRefNames' _ = []    
