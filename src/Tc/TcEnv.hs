
module Tc.TcEnv where

import Data.List (union)
import Language.Haskell.Exts
import qualified Text.PrettyPrint.HughesPJ as P 

import BuiltIn.InitialDataConEnv

import Tc.Assumption
import Tc.Class
import Tc.TcSubst
import Tc.TcLabel

import Utils.Env
import Utils.EnvMonad
import Utils.ErrMsg hiding (empty)
import Utils.Id
import qualified Utils.Stack as S

-- this module defines the
-- environment used by the
-- type checking / inference
-- engine.

-- an environment for class / instance constraints

type ClassEnv = Env Class

-- an environment for variable assumptions

type VarEnv = Env Assumption

-- the definition of the type checking environment

data TcEnv = TcEnv {
                classenv :: ClassEnv,
                varenv :: S.Stack VarEnv,
                labelenv :: LabelEnv,
                diag :: S.Stack Diagnostic,
                fresh :: Int,
                subst :: Subst
             } deriving (Show)
             
-- generating an environment

mkTcEnv :: ClassEnv -> [Assumption] -> LabelEnv -> TcEnv
mkTcEnv clsenv as lbl
    = TcEnv clsenv s lbl S.emptyStack 0 nullSubst
      where
         s = S.singleton (foldr f empty (as ++ initialVarEnv))
         f a@(i :>: _) ac = insert i a ac
                       

-- the TcEnv supports diagnostic messages
 
instance DiagnosticM TcEnv where
    pushDiagnostic d = do
                         s <- get
                         put (s {diag = S.push d (diag s) })
    popDiagnostic = do
                        e <- get
                        s <- gets (S.pop . diag)
                        case s of
                            Nothing -> return P.empty -- XXX
                            Just (x, ns) -> put (e {diag = ns}) >> return x 
    size = gets (S.stackSize . diag)  
    
    
lookupVar :: Id -> TcEnv -> Maybe Assumption
lookupVar i e 
    = f (S.toList $ varenv e)
      where
        f [] = Nothing
        f (s:ss) 
            = case lookupEnv i s of
                    Just a -> Just a
                    Nothing -> f ss 
        
labelsFrom :: Id -> TcEnv -> Maybe [Label]
labelsFrom i = lookupEnv i . labelenv

-- this instance sucks!

instance Substitutable TcEnv where
    apply s e = e{varenv = S.fromList $ map (foldrWithId (\k v ac -> insert k (apply s v) ac) empty) 
                           (S.toList $ varenv e)}
    tv e = foldr (union . tv . image) [] (S.toList $ varenv e)  
