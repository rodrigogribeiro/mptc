
module Tc.TcLiteral where

import Language.Haskell.Exts

import BuiltIn.BuiltInTypes
import Tc.TcMonad

import Utils.ErrMsg

-- type checking literals

tcLiteral :: Literal -> TcM (Context, Type)
tcLiteral (Char _) = return ([], tChar)
tcLiteral (String _) = return ([], tString)
tcLiteral (Int _) = do
                       v <- newFreshVar
                       return ([numconstr v], v)
tcLiteral (Frac _) = do
                       v <- newFreshVar
                       return ([fracconstr v], v) 
tcLiteral x = unsupportedExpErrMsg x                       