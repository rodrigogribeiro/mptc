module Tc.TcExp where

import Control.Monad.Trans
import Control.Monad (when)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.List(nub)
import Language.Haskell.Exts

import BuiltIn.BuiltInTypes
import Tc.Assumption
import Tc.TcEnv
import Tc.TcLiteral
import Tc.TcMonad
import Tc.TcPat
import Tc.TcSubst
import Tc.TcAlphaEq
import Tc.TcWellformed
 
import Utils.Id
import Utils.ErrMsg

-- type checking expressions

tcExp :: Exp -> TcM (Context, Type)
tcStmt :: Stmt -> TcM (Context, [Assumption], Maybe Type)

