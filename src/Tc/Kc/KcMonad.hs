
module Tc.Kc.KcMonad where

import Language.Haskell.Exts

import Tc.Kc.KcEnv
import Tc.Kc.KcSubst

import Utils.EnvMonad
import Utils.ErrMsg
import Utils.Id
import Utils.Stack

-- this module defines a monad for the
-- kind inference process.

-- defining a data type for the kind
-- checking / inference algorithm

data KcState = KcState {
                  env   :: KcEnv,
                  subst :: KindSubst,
                  fresh :: Int,
                  diag  :: Stack Diagnostic
               }

-- adding context diagnostic to the kind inference
-- algorithm state.

instance DiagnosticM KcState where
    pushDiagnostic d = do
                         s <- get
                         put (s {diag = push d (diag s) })
    popDiagnostic = do
                        e <- get
                        s <- gets (pop . diag)
                        case s of
                            Nothing -> diagnosticStackPanic
                            Just (x, ns) -> put (e {diag = ns}) >> return x
    size = gets (stackSize . diag)



emptyState :: KcEnv -> KcState
emptyState e = KcState e nullSubst 0 emptyStack

-- monad definition

type Kc a = EnvM KcState a

runKc :: KcEnv -> Kc a -> IO (Either String a, KcEnv)
runKc e m = do
              (r, e') <- run (emptyState e) m
              return (r, env e')

-- some functions over the monad

freshVar :: Kc Kind
freshVar = do
            k <- gets newVar
            modify incFresh
            return k

getTyVarsKind :: Kc [(Id, Kind)]
getTyVarsKind = gets (getTyVarsInEnv . env)

getSubst :: Kc KindSubst
getSubst = gets subst

extSubst :: KindSubst -> Kc ()
extSubst s
    = do
        e <- get
        put (e {subst = s @@ (subst e)})

-- dealing with type environment

getKcEnv :: Kc KcEnv
getKcEnv = gets env

addClassKind :: Id -> Kind -> Kc ()
addClassKind i k = modify (\s -> s{env = insertClassKind i k (env s)})

addTypeKind :: Id -> Kind -> Kc ()
addTypeKind i k = modify (\s -> s{env = insertTypeKind i k (env s)})

getClassKind :: Id -> Kc (Maybe Kind)
getClassKind i = gets (lookupClassKind i . env)

getTypeKind :: Id -> Kc (Maybe Kind)
getTypeKind i = gets (lookupTypeKind i . env)

getTyVarKind :: Id -> Kc (Maybe Kind)
getTyVarKind i = gets (lookupTyVarKind i . env)

-- the unification algorithm

mgu :: Kind -> Kind -> Kc KindSubst
mgu KindStar KindStar = return nullSubst
mgu (KindVar v) u = varBind v u
mgu u (KindVar v) = varBind v u
mgu (KindFn l1 r1) (KindFn l2 r2)
                      = do
                          s1 <- mgu l1 l2
                          s2 <- mgu (apply s1 r1)
                                    (apply s1 r2)
                          return (s2 @@ s1)
mgu (KindParen l) k = mgu l k
mgu k (KindParen l) = mgu k l
mgu x y = unificationError x y

varBind :: KVar -> Kind -> Kc KindSubst
varBind v k
        | (KindVar v) == k = return nullSubst
        | v `elem` (vars k) = occursCheckError v k
        | otherwise = return [(v,k)]

unify :: Kind -> Kind -> Kc ()
unify k1 k2 = do
                s <- getSubst
                s' <- mgu (apply s k1)
                          (apply s k2)
                extSubst s'


-- some auxiliar functions

incFresh :: KcState -> KcState
incFresh s = s{fresh = (fresh s) + 1}

newVar :: KcState -> Kind
newVar s = KindVar (UnQual $ Ident ("$" ++ show (fresh s)))
