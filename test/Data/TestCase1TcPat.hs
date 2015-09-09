
module Tests.Data.TestCase1TcPat where


import Language.Haskell.Exts

import BuiltIn.BuiltInTypes
import Tc.Assumption
import Tc.Class
import qualified Utils.Env as Env
import Utils.Id

a = TyVar (Ident "$0")

x = PVar (Ident "x")
xs = PVar (Ident "xs")
l = PVar (Ident "l")
r = PVar (Ident "r")

xi = toId (Ident "x")
li = toId (Ident "l")
ri = toId (Ident "r")
xsi = toId (Ident "xs")

test1 = (x, ([], [xi :>: a], a))
test2 = (PLit (Char 'c'), ([], [], tChar))
test3 = (PNeg x, ([], [xi :>: a], a))
test4 = (PNPlusK (Ident "n") 1, ([integralconstr a], [(toId (Ident "n")) :>: a], a))
test5 = (PInfixApp x (Special Cons) xs, ([], [xi :>: a, xsi :>: (tList a)], tList a))
test6 = (PApp (UnQual (Ident "Node")) [x, l, r], ([], [xi :>: a, li :>: (tTree a), ri :>: (tTree a)], tTree a))
test7 = (PTuple Boxed [x, PLit (Char 'c')], ([], [xi :>: a], TyTuple Boxed [a, tChar]))
test8 = (PList [x, PLit (Char 'c')], ([], [xi :>: tChar], TyList tChar))
test9 = (PRec (UnQual (Ident "Foo")) [PFieldPat (UnQual (Ident "foo")) (PLit (Int 0))], ([cnint], [], TyCon (UnQual (Ident "Foo"))))
test10 = (PRec (UnQual (Ident "Foo")) [PFieldPun (Ident "foo")], ([], [cfoo :>: tint], (TyCon (UnQual (Ident "Foo")))))
test11 = (PRec (UnQual (Ident "Foo")) [PFieldWildcard], ([],[],(TyCon (UnQual (Ident "Foo")))))

-- to be included on gamma

cons = (toId (Special Cons), toId (Special Cons) :>: (TyFun a (TyFun (tList a) (tList a))))
node = (toId (UnQual (Ident "Node")), toId (UnQual (Ident "Node")) :>: (TyFun a (TyFun (tTree a) (TyFun (tTree a) (tTree a)))))
foo = (toId (UnQual (Ident "foo")), toId (UnQual (Ident "foo")) :>: TyFun (TyCon (UnQual (Ident "Foo"))) (TyCon (UnQual (Ident "Int"))))
fooc = (tfoo, tfoo :>: (TyFun (TyCon (UnQual (Ident "Int")))) (TyCon (UnQual (Ident "Foo"))))

tfoo = toId (UnQual (Ident "Foo"))
tint = TyCon (UnQual (Ident "Int"))
cfoo = toId (UnQual (Ident "foo"))

cnint = ClassA (UnQual (Ident "Num")) [tint]

lblenv = Env.fromList [(tfoo, [(cfoo, tint)])]

tTree t = TyApp (TyCon (UnQual (Ident "Tree"))) t
