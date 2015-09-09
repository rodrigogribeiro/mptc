module Tests.Data.TestCase1Sat where

import Language.Haskell.Exts
import BuiltIn.BuiltInTypes

import Tc.Class
import qualified Utils.Env as Env
import Utils.Id
import Utils.Stack

-- test one

c = UnQual (Ident "C")

t = TyCon (UnQual (Ident "T"))

i = TyCon (UnQual (Ident "Int"))
b = TyCon (UnQual (Ident "Bool"))
ch = TyCon (UnQual (Ident "Char"))
a = TyVar (Ident "$a")
a1 = TyVar (Ident "$a1")
b1 = TyVar (Ident "$b")
b2 = TyVar (Ident "b")
a2 = TyVar (Ident "a")
t1 = TyApp (TyApp t t) a2

as = ClassA c [a,b1]

inst1 = Inst (toId c) [i,b] []
inst2 = Inst (toId c) [ch,i] []
inst3 = Inst (toId c) [t1,b2] []

clsc = Class (toId c) [TyVar (Ident "a"), TyVar (Ident "b")] [] [] [inst1, inst2, inst3]

gamtest1 = singleton $ Env.empty
clstest1 = Env.fromList [(toId c,clsc)]
kappatest1 = [ClassA c [a1, TyApp t a1]]

test1 = (clstest1, gamtest1, kappatest1, True)

-- test2

test2 = (clstest2, gamtest2, kappatest2, True)

clstest2 = Env.fromList [(toId s, clss), (toId r, clsr)]
gamtest2 = singleton $ Env.empty

s = UnQual (Ident "Show")
r = UnQual (Ident "Read")

clss = Class (toId s) [TyVar (Ident "a")] [] [] [insts1]
clsr = Class (toId r) [TyVar (Ident "a")] [] [] [instr1]

insts1 = Inst (toId s) [i] []
instr1 = Inst (toId r) [i] []

kappatest2 = [ClassA s [a], ClassA r [a]]

-- test3

test3 = (clstest2, gamtest2, kappatest3, True)

kappatest3 = [ClassA r [i]]

-- test4 pcp

test4 = (envclspcp, gamtest2, kappapcp, False)

envclspcp = Env.fromList [(toId x, pcpcls)]

kappapcp = [ClassA x [ma,mb]]

ma = TyVar (Ident "$a")
mb = TyVar (Ident "$b")

x = UnQual (Ident "x")

pcpcls = Class (toId x) [TyVar (Ident "a"), TyVar (Ident "b")] [] [] [instpcp1, instpcp2,
                                                                      instpcp3, instpcp4,
                                                                      instpcp5, instpcp6]

z = TyCon (UnQual (Ident "0"))
u = TyCon (UnQual (Ident "1"))

instpcp1 = Inst (toId x) [TyFun u (TyFun z z), u] []
instpcp2 = Inst (toId x) [TyFun u (TyFun z (TyFun z (TyVar (Ident "a")))), 
                          TyFun u (TyVar (Ident "b"))] [ClassA x [TyVar (Ident "a"), TyVar (Ident "b")]]
                          
instpcp3 = Inst (toId x) [z , TyFun u (TyFun z z)] []
instpcp4 = Inst (toId x) [TyFun z (TyVar (Ident "a")), 
                          TyFun u (TyFun z (TyFun z (TyVar (Ident "b"))))]
                         [ClassA x [TyVar (Ident "a"), TyVar (Ident "b")]]
                         
instpcp5 = Inst (toId x) [TyFun (TyFun z z) u] []
instpcp6 = Inst (toId x) [TyFun u (TyFun z (TyFun z (TyVar (Ident "a")))), 
                          TyFun u (TyVar (Ident "b"))] [ClassA x [TyVar (Ident "a"), TyVar (Ident "b")]]                                                 


-- test5

test5 = (envcls5, gamtest2, kappa5, True)

envcls5 = Env.fromList [(icn, cc), (idn, dc)]

cn = UnQual (Ident "C")
dn = UnQual (Ident "D")

icn = toId cn
idn = toId dn

cc = Class icn [(TyVar (Ident "a")), (TyVar (Ident "b"))] [] [] [icib, icic]
dc = Class idn [(TyVar (Ident "a"))] [] [] [idi]

kappa5 = [ClassA cn [(TyVar (Ident "$0")), (TyVar (Ident "$1"))], ClassA dn [(TyVar (Ident "$0"))]] 

icib = Inst icn [tInt, tBool] []
icic = Inst icn [tInt, tChar] []

idi = Inst idn [tInt] []

-- test 6

test6 = (envcls6, gamtest2, kappa5, True)


envcls6 = Env.fromList [(icn, cc'), (idn, dc')] 

cc' = Class icn [(TyVar (Ident "a")), (TyVar (Ident "b"))] [] [] [icib]
dc' = Class idn [(TyVar (Ident "a"))] [] [] [idi]


-- test 7

test7 = (envcls7,gamtest2,kappa7,False)

cc7 = Class icn [(TyVar (Ident "a")), (TyVar (Ident "b"))] [] [] [i7]

i7 = Inst icn [TyList (TyVar (Ident "a")), TyVar (Ident "b")] 
			  [ClassA cn [TyList (TyList (TyVar (Ident "a"))), (TyVar (Ident "b"))]]

envcls7 = Env.fromList [(icn,cc7)]
kappa7 = [ClassA cn [TyList (TyCon (UnQual (Ident "Int"))), TyCon (UnQual (Ident "Int"))]]

-- test 8

test8 = (envcls8, gamtest2, kappa8, True)

envcls8 = Env.fromList [(icn,cc8)]

cc8 = Class icn [(TyVar (Ident "a")), (TyVar (Ident "b"))] [] [] [i81,i82]

i81 = Inst icn [(TyList (TyVar (Ident "a"))), TyCon (UnQual (Ident "Int"))] []
i82 = Inst icn [TyVar (Ident "a"), TyList (TyVar (Ident "b"))] 
			   [ClassA (UnQual (Ident "C")) [TyList (TyList (TyVar (Ident "a"))), TyVar (Ident "b")]] 


kappa8 = [ClassA cn [TyCon (UnQual (Ident "Int")), TyList $ TyList $ TyList $ TyList (TyCon (UnQual (Ident "Int")))]]

-- test 9

test9 = (envcls9, gamtest2, kappa9, True)

envcls9 = Env.fromList [(icn,cc9)]

cc9 = Class icn [(TyVar (Ident "a")), (TyVar (Ident "b"))] [] [] [i91,i92]

i91 = Inst icn [TyApp t va, ti] []
i92 = Inst icn [va, TyApp t vb] [ClassA cn [TyApp t (TyApp t va), vb]]

kappa9 = [ClassA cn [ti, TyApp t (TyApp t (TyApp t ti))]]

va = TyVar (Ident "a")
vb = TyVar (Ident "b")

ti = TyCon (UnQual (Ident "Int"))
tflo = TyCon (UnQual (Ident "Float"))

-- test 10

test10 = (envcls10, gamtest2, kappa10, False)

envcls10 = Env.fromList [(icn,cc10)]

cc10 = Class icn [(TyVar (Ident "a")), (TyVar (Ident "b"))] [] [] [i101]

i101 = Inst icn [(TyApp t va)] [ClassA cn [TyApp t (TyApp t va)]]

kappa10 = [ClassA cn [TyApp t ti]]

-- test 11

test11 = (envcls11, gamtest2, kappa11, True)

envcls11 = Env.fromList [(icn,cc11)]

kappa11 = [ClassA cn [(TyApp t (TyApp t ti)), tflo]]

cc11 = Class icn [(TyVar (Ident "a")), (TyVar (Ident "b"))] [] [] [i111, i112]

i111 = Inst icn [ti, (TyApp t (TyApp t tflo))] []
i112 = Inst icn [(TyApp t va), vb] [ClassA cn [va, TyApp t vb]]
