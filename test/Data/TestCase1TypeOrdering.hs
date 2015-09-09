
module Tests.Data.TestCase1TypeOrdering where

import Language.Haskell.Exts

import Tc.TcEnv
import qualified Utils.Env as Env
import Utils.Stack

testcase1 = (delta1, delta2, False)
testcase2 = (delta2, delta1, False)
testcase3 = (delta1, delta1, True)
testcase4 = (a, qva, False)
testcase5 = (qva, a, True)
testcase6 = (qta, ta, True)
testcase7 = (a, t, True)
testcase8 = (t, a, False)
testcase9 = (qta, ta', True)
testcase10 = (t11, t11, True)
 
qta = TyForall Nothing [] (TyFun qva qva)

ta = TyForall Nothing [] (TyFun t t)

ta' = TyForall Nothing [] (TyFun qva t)

delta1 = ClassA c [a1, ty1]
delta2 = ClassA c [a, ty2]

c = UnQual (Ident "C")

t = TyCon (UnQual (Ident "T"))

a = TyVar (Ident "$a")
a1 = TyVar (Ident "$a1")
b1 = TyVar (Ident "$b1")

qva = (TyVar (Ident "a"))

t11 = TyFun (TyFun a1 b1) (TyFun a1 b1)

testenv = TcEnv Env.empty (singleton Env.empty) Env.empty emptyStack 0 []

ty1 = TyApp (TyApp (TyApp t t) t) a1

ty2 = TyApp t a