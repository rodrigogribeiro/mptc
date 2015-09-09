
module Tc.TcPat where

import Control.Monad (when)
import Control.Monad.Trans
import Data.List(union)
import Data.Maybe(isNothing, fromJust)
import Language.Haskell.Exts

import BuiltIn.BuiltInTypes
import Tc.Assumption
import Tc.TcLabel
import Tc.TcLiteral
import Tc.TcMonad
import Tc.TcSubst
import Utils.ErrMsg
import Utils.Id


-- type checking patterns

tcPats :: [Pat] -> TcM (Context, [Assumption], [Type])
tcPats ps = do
                r <- mapM tcPat ps
                let
                   ctx = concat [c | (c,_,_) <- r]
                   as  = concat [a | (_,a,_) <- r]
                   ts  = [t | (_, _, t) <- r]
                return (ctx, as, ts)
                
-- type checking one pattern                

tcPat :: Pat -> TcM (Context, [Assumption], Type)
tcPat p@(PVar n) 
    = withDiagnostic p $ 
        do
          let i = toId n
          v <- newFreshVar
          return ([], [i :>: v], v)
tcPat p@(PLit l)
    = withDiagnostic p $ 
        do
          (ctx,t) <- tcLiteral l
          return (ctx, [], t)     
tcPat p@(PNeg p')
    = withDiagnostic p $ tcPat p'
tcPat p@(PNPlusK p' i)
    = withDiagnostic p $
        do
          t <- newFreshVar
          let 
             i = toId p'
             ctx' = [integralconstr t]
          return (ctx', [i :>: t] , t) 
tcPat p@(PInfixApp pl n pr) 
    = withDiagnostic p $
        do
          (cl, al, tl) <- tcPat pl
          (cr, ar, tr) <- tcPat pr
          t' <- newFreshVar
          (_ :>: tn) <- freshInst =<< lookupGamma (toId n) 
          let (ctx', tn') = splitType tn
          unify tn' (foldr TyFun t' [tl, tr])
          s <- getSubst
          return (apply s (ctx' `union` cl `union` cr), apply s (al ++ ar), apply s t')
tcPat p@(PApp n ps)
    = withDiagnostic p $ 
        do
          (ctx, as, ts) <- tcPats ps
          t' <- newFreshVar
          (_ :>: tn) <- freshInst =<< lookupGamma (toId n)
          let (ctx', tn') = splitType tn
          unify tn' (foldr TyFun t' ts)
          s <- getSubst
          return (apply s (ctx `union` ctx'), apply s as, apply s t')
tcPat p@(PTuple _ ps)
    = withDiagnostic p $ 
        do
          (ctx, as, ts) <- tcPats ps
          return (ctx, as, TyTuple Boxed ts)
tcPat p@(PList ps)
    = withDiagnostic p $ 
        do
          (ctx, as,ts) <- tcPats ps
          t <- newFreshVar
          mapM_ (unify t) ts
          s <- getSubst
          return (apply s ctx, apply s as, apply s (TyList t))
tcPat p@(PParen p') = withDiagnostic p $ tcPat p'
tcPat p@(PRec n fs) 
    = withDiagnostic p $
        do
          lbls <- lookupLabels (toId n)
          (ctx,as,ts) <- tcPatFields lbls fs
          (_ :>: tn) <- freshInst =<< lookupGamma (toId n)
          let (ctx', tn') = splitType tn
          t <- newFreshVar
          unify tn' (foldr TyFun t ts)
          s <- getSubst
          return (apply s (ctx `union` ctx'), apply s as, apply s t)         
tcPat p@(PAsPat n p') 
    = withDiagnostic p $
        do
         (ctx,as, t) <- tcPat p'
         return (ctx, ((toId n) :>: t : as), t)
tcPat PWildCard
    = do
        t <- newFreshVar
        return ([], [], t)
tcPat p@(PIrrPat p')
    = withDiagnostic p $ tcPat p'
tcPat p@(PatTypeSig _ p' ty)
    = withDiagnostic p $
        do 
          (ctx, as, t) <- tcPat p'
          unify ty t
          return (ctx, as, t)
tcPat x = unsupportedPatErrMsg x        
      

-- typing field patterns      
      
      
tcPatFields :: [Label] -> [PatField] -> TcM (Context, [Assumption], [Type])                    
tcPatFields lbls fs 
    = do
        r <- mapM (tcPatField lbls) fs
        let
           ctx = concat [c | (c,_,_) <- r]
           as  = concat [a | (_,a,_) <- r]
           ts  = [t | (_, _, t) <- r]
        return (ctx, as, ts)     
        
        
        
tcPatField :: [Label] -> PatField -> TcM (Context, [Assumption], Type)
tcPatField lbls (PFieldPat n p)
    = do
        (ctx, as, t) <- tcPat p
        let
          ty = lookup (toId n) lbls
        when (isNothing ty) (notDefinedError n)
        ty' <- freshInst (fromJust ty)
        let (ctx',t') = splitType ty'
        unify t t'
        s <- getSubst
        return (apply s ctx, apply s as, apply s t)  
tcPatField lbls (PFieldPun n) 
    = do
        let 
            ty = lookup (toId n) lbls
        when (isNothing ty) (notDefinedError n)
        ty1 <- freshInst (fromJust ty)  
        let (ctx, ty1') = splitType ty1
        return (ctx, [(toId n) :>: ty1], ty1')        
tcPatField _ PFieldWildcard
    = do
        t <- newFreshVar
        return ([],[],t)                       

-- some auxiliar functions
                           
constructor (TyFun _ r) = constructor r
constructor t = t        

splitType :: Type -> (Context, Type)
splitType (TyForall m ctx ty) = (ctx, ty)
splitType t = ([], t)             
