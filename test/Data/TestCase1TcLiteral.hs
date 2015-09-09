
module Tests.Data.TestCase1TcLiteral where

import Language.Haskell.Exts

import BuiltIn.BuiltInTypes
import Tc.Class
import qualified Utils.Env as Env
import Utils.Id

-- test1

a = TyVar (Ident "$0")

test1 = (Char 'c', ([], tChar))
test2 = (String "", ([], tString))
test3 = (Int 0, ([numconstr a], a))
test4 = (Frac 0.6, ([fracconstr a], a)) 

