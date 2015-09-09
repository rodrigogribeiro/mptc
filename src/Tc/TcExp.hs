
module Tc.TcExp where

import Control.Monad.Trans
import Control.Monad (liftM, when)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.List(nub)
import Language.Haskell.Exts

import BuiltIn.BuiltInTypes
import Tc.Assumption
import Tc.TcDecl
import Tc.TcLabel
import Tc.TcLiteral
import Tc.TcMonad
import Tc.TcPat
import Tc.TcSubst
 
import Utils.Id
import Utils.ErrMsg



-- type checking expressions

tcExp :: Exp -> TcM (Context, Type)
tcExp e@(Var v) 
    = withDiagnostic e $ 
        do
          (_ :>: ty) <- freshInst =<< lookupGamma (toId v)
          return (splitType ty)
tcExp e@(Con n)
    = withDiagnostic e $ 
        do
          (_ :>: ty) <- freshInst =<< lookupGamma (toId n)
          return (splitType ty)
tcExp e@(Lit l)
    = withDiagnostic e $ tcLiteral l
tcExp e@(InfixApp l n r)
    = withDiagnostic e $ 
        do
          (ctxl, tl) <- tcExp l
          (ctxr, tr) <- tcExp r
          (_ :>: ty) <- freshInst =<< lookupGamma (toId n)
          let (ctx,ty') = splitType ty
          t <- newFreshVar
          unify ty' (TyFun tl (TyFun tr t))
          s  <- getSubst   
          let k = apply s $ concat [ctxl, ctxr, ctx]               
          return (k, apply s t)
tcExp e@(App l r)
    = withDiagnostic e $ 
        do
          (ctxl, tl) <- tcExp l
          (ctxr, tr) <- tcExp r
          t <- newFreshVar
          unify (TyFun tr t) tl
          s <- getSubst
          let k = apply s (ctxl ++ ctxr)
          return (k, apply s t)
tcExp e@(NegApp e')
    = withDiagnostic e $ 
        do 
          (ctx, te) <- tcExp e' 
          return (((numconstr te):ctx), te)   
tcExp e@(Lambda _ pats e')
    = withDiagnostic e $
        do
          (ctx, as, ts) <- tcPats pats 
          (ctx',te') <- extendGamma as (tcExp e')
          let t = foldr TyFun te' ts
          s <- getSubst
          let k = apply s (ctx ++ ctx')
              t' = apply s t
          return (k,t')  
tcExp e@(Let bs e') 
    = withDiagnostic e $ 
        do
           (ctx,as) <- tcBinds False bs
           (ctx',t) <- extendGamma as (tcExp e')
           s <- getSubst
           let k = apply s (ctx ++ ctx')
               t' = apply s t
           return (k,t') 
tcExp e@(If e1 e2 e3)
    = withDiagnostic e $ 
        do
          (ctx1, t1) <- tcExp e1
          (ctx2, t2) <- tcExp e2
          (ctx3, t3) <- tcExp e3
          unify tBool t1
          unify t2 t3
          s <- getSubst
          let ctx = apply s (concat [ctx1, ctx2, ctx3])
              t' = apply s t2
          return (ctx, t2)
tcExp e@(Case e' alts)
    = withDiagnostic e $ 
        do
          (ctxe, t2) <- tcExp e'
          (ctxa, tas) <- tcAlts alts
          t' <- newFreshVar
          unify (TyFun t2 t') tas
          s <- getSubst
          let k = apply s (ctxe ++ ctxa)
              t'' = apply s t'
          return (k,t'')
tcExp e@(Do stmts)
    = withDiagnostic e $
        do 
          (ctx, as, t) <- tcStmts stmts
          when (isNothing t) (lastStatementExpressionError e)
          t' <- newFreshVar
          t'' <- newFreshVar
          let ty = TyApp t' t''
          unify ty (fromJust t)
          s <- getSubst
          let k = apply s ctx
              t1 = apply s ty 
          return (k,t1)    
tcExp e@(Tuple _ es)
    = withDiagnostic e $ 
        do
          r <- mapM tcExp es
          s <- getSubst
          let 
            ctx = apply s (concatMap fst r)
            ts  = apply s (map snd r)
          return (ctx, TyTuple Boxed ts)               
tcExp e@(List es)
    = withDiagnostic e $ 
        do
          r <- mapM tcExp es
          let
            ctx = concatMap fst r
            ts  = map snd r
          t <- newFreshVar
          mapM (unify t) ts
          s <- getSubst
          return (apply s ctx, apply s (TyList t))
tcExp e@(Paren e')
    = withDiagnostic e $ tcExp e'
tcExp e@(LeftSection e' n)
    = withDiagnostic e $ 
        do
          (ctx,t) <- tcExp e'
          (_ :>: ty) <- freshInst =<< lookupGamma (toId n)
          let (ctx', t') = splitType ty
          v <- newFreshVar
          unify (TyFun t v) t'
          s <- getSubst
          let k = apply s (ctx ++ ctx')
          return (k, apply s v)       
tcExp e@(RightSection n e')
    = withDiagnostic e $ 
        do
          (ctx,t) <- tcExp e'
          (_ :>: ty) <- freshInst =<< lookupGamma (toId n)
          let (ctx', t') = splitType ty
          v <- newFreshVar
          v' <- newFreshVar
          unify (TyFun v (TyFun t v')) t'
          s <- getSubst
          let k = apply s (ctx ++ ctx')            
          return (k, apply s (TyFun v v'))
tcExp e@(RecConstr n upds)
    = withDiagnostic e $ 
        do
          (_ :>: ty) <- freshInst =<< lookupGamma (toId n)
          let (ctx', t') = splitType ty
          lbls <- lookupLabels (toId n)
          (ctx, ts) <- tcFieldUpdates lbls upds
          s <- getSubst
          let k = apply s ctx' ++ ctx
          return (k, apply s $ constructor t')
tcExp e@(RecUpdate e' upds)
    = withDiagnostic e $ 
        do
          (ctx, t) <- tcExp e'
          n <- typeConstructorName (constructor t)
          lbls <- lookupLabels (toId n)
          (ctx1, ts) <- tcFieldUpdates lbls upds
          s <- getSubst
          let k = apply s (ctx ++ ctx1)
          return (k, apply s $ constructor t)    
tcExp e@(EnumFrom e')
    = withDiagnostic e $ 
        do
          (ctx,t) <- tcExp e'
          s <- getSubst
          let k = apply s ((enumconstr t):ctx)
          return (k, apply s (TyList t))
tcExp e@(EnumFromTo e1 e2)
    = withDiagnostic e $ 
        do
          (ctx,t) <- tcExp e1                    
          (ctx',t') <- tcExp e2
          unify t t'
          s <- getSubst
          let k = apply s ((map enumconstr [t,t']) ++ ctx ++ ctx')
          return (k, apply s (TyList t))
tcExp e@(EnumFromThen e1 e2)
    = withDiagnostic e $ 
        do
          (ctx,t) <- tcExp e1         
          (ctx',t') <- tcExp e2
          unify t t'
          s <- getSubst
          let k = apply s ((map enumconstr [t,t']) ++ ctx' ++ ctx)
          return (k, apply s (TyList t))
tcExp e@(EnumFromThenTo e1 e2 e3)
    = withDiagnostic e $ 
        do
          (ctx,t) <- tcExp e1
          (ctx',t') <- tcExp e2
          (ctx'',t'') <- tcExp e3
          unify t t'
          unify t' t''
          s <- getSubst
          let c = apply s (concat [map enumconstr [t,t',t''], ctx, ctx', ctx''])
          return (c, apply s (TyList t))
tcExp e@(ListComp e' quals)
    = withDiagnostic e $
        do
          (ctx,as) <- tcQuals quals
          (ctx',t) <- extendGamma as (tcExp e')
          s <- getSubst
          let k = apply s (ctx ++ ctx')
          return (k, apply s (TyList t))     
tcExp e@(ExpTypeSig _ e' ty)
    = withDiagnostic e $ 
        do
          ty' <- freshInst ty
          (ctx,t) <- tcExp e'
          let (ctx', t') = splitType ty'
          matchfy t t'
          s <- getSubst
          let k = apply s (ctx ++ ctx')
          return (k, apply s t')   
tcExp x = unsupportedExpErrMsg x                              
                
-- type checking alternatives

tcAlts :: [Alt] -> TcM (Context, Type)
tcAlts alts 
    = do
        r <- mapM tcAlt alts
        let
           ctx = concatMap fst r
           ts  = map snd r
        t' <- newFreshVar
        mapM (unify t') ts
        s <- getSubst
        return (apply s ctx, apply s t')
        
        
tcAlt :: Alt -> TcM (Context, Type)
tcAlt a@(Alt _ p galts wbinds)
    = withDiagnostic a $ 
        do
          (ctx,as',t) <- tcPat p        
          (ctx0, as) <- extendGamma as' (tcBinds False wbinds)
          (ctx',t') <- extendGamma (as ++ as') (tcGuardedAlts galts)
          s <- getSubst
          let k = apply s (ctx0 ++ ctx ++ ctx')
          return (k, apply s (TyFun t t')) 
   
        
tcGuardedAlts :: GuardedAlts -> TcM (Context, Type)
tcGuardedAlts (UnGuardedAlt e) 
    = tcExp e
tcGuardedAlts (GuardedAlts alts) 
    = do
        r <- mapM tcGuardedAlt alts
        let
            ctx = concatMap fst r
            ts  = map snd r
        t <- newFreshVar
        mapM (unify t) ts
        s <- getSubst
        return (apply s ctx, apply s t)
        
        
tcGuardedAlt :: GuardedAlt -> TcM (Context,Type)
tcGuardedAlt (GuardedAlt _ stmts e)
    = do
        (ctx,as, t) <- tcStmts stmts
        (ctx',t') <- tcExp e
        return (ctx ++ ctx', t')         
                                  
                                                 
-- type checking statements

tcStmts :: [Stmt] -> TcM (Context, [Assumption], Maybe Type)
tcStmts stmts 
    = tcStmts' stmts tcStmt ([],[], Nothing) 
            
tcStmts' :: [Stmt] -> (Stmt -> TcM (Context, [Assumption], Maybe Type)) -> (Context, [Assumption], Maybe Type) -> TcM (Context, [Assumption], Maybe Type)
tcStmts' [] _ ac = return ac
tcStmts' (s:ss) f (c,a,mt) 
    = do
        (ctx,as,t) <- f s
        extendGamma as (tcStmts' ss f (c ++ ctx, as ++ a,t))                    
        
-- it will return a nothing, if it's not an expression        
        
tcStmt :: Stmt -> TcM (Context, [Assumption], Maybe Type)
tcStmt s@(Generator _ p e)
    = withDiagnostic s $ 
        do
          (ctx,as,t) <- tcPat p
          (ctx',t')  <- tcExp e
          v' <- newFreshVar
          v'' <- newFreshVar
          unify (TyApp v' v'') t'
          s <- getSubst
          return (apply s (ctx ++ ctx'), as, Nothing)
tcStmt s@(Qualifier e)
    = withDiagnostic s $
        do
          (ctx,t) <- tcExp e
          return (ctx, [], Just t)
tcStmt s@(LetStmt binds)
    = withDiagnostic s $ 
        do
          (ctx,as) <- tcBinds False binds
          return (ctx, as, Nothing)
tcStmt x = unsupportedExpErrMsg x                           
               

-- type checking field updates
tcFieldUpdates :: [Label] -> [FieldUpdate] -> TcM (Context, [Type])
tcFieldUpdates lbls fupds 
    = do
        r <- mapM (tcFieldUpdate lbls) fupds
        let 
           ctx = concatMap fst r
           ts  = map snd r
        return (ctx, ts)      
        
        
tcFieldUpdate :: [Label] -> FieldUpdate -> TcM (Context, Type)
tcFieldUpdate lbls (FieldUpdate n e)
    = do
        (ctx,t) <- tcExp e
        let ty = lookup (toId n) lbls
        when (isNothing ty) (notDefinedError n)
        ty' <- freshInst (fromJust ty)
        let (ctx',t') = splitType ty'
        return (ctx', t')    
tcFieldUpdate lbls (FieldPun n)
    = do
        let 
            ty = lookup (toId n) lbls
        when (isNothing ty) (notDefinedError n)
        ty1 <- freshInst (fromJust ty)  
        let (ctx, ty1') = splitType ty1
        return (ctx, ty1')
tcFieldUpdate lbls FieldWildcard
    = do 
        t <- newFreshVar
        return ([],t)                                       
                                         

-- type checking list comprehensions qualifiers

tcQuals :: [QualStmt] -> TcM (Context, [Assumption])
tcQuals qs 
    = do
        (ctx, as, _) <- tcStmts' (map unQual qs) tcQStmt ([],[], Nothing) 
        return (ctx, as)
        
unQual (QualStmt x) = x
unQual x = unsupportedExpErrMsg x   

tcQStmt :: Stmt -> TcM (Context, [Assumption], Maybe Type)
tcQStmt s@(Generator _ p e)
    = withDiagnostic s $ 
        do
          (ctx,as,t) <- tcPat p
          (ctx',t')  <- tcExp e
          unify (TyList t) t'
          s <- getSubst
          return (apply s (ctx ++ ctx'), apply s as, Nothing)           
tcQStmt s@(Qualifier e)
    = withDiagnostic s $
        do
          (ctx,t) <- tcExp e
          unify t tBool
          s <- getSubst
          return (apply s ctx, [], Just (apply s t))
tcQStmt s@(LetStmt binds)
    = withDiagnostic s $ 
        do
          (ctx,as) <- tcBinds False binds
          return (ctx, as, Nothing)
tcQStmt x = unsupportedExpErrMsg x                        

-- some auxiliar functions

       

typeFromAssumption (_ :>: ty) = ty

typeConstructorName :: Type -> TcM QName
typeConstructorName (TyApp l _) = typeConstructorName l
typeConstructorName (TyCon n) = return n
typeConstructorName x = noTypeConstructorExpression x   

                   
