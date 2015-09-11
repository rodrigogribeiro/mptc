
module Data.TestCase1TcExp where


import Language.Haskell.Exts hiding (loc)

import BuiltIn.BuiltInTypes
import Tc.Assumption
import Tc.Class
import qualified Utils.Env as Env
import Utils.Id
import Utils.Stack



-- test 1 : variables

test1 = (x, ([], tInt))

x = Var (UnQual (Ident "y"))

idx = toId (UnQual (Ident "y"))

-- test 2 : data constructors

test2 = (x1, ([], tla'))

x1 = Con (Special ListCon)

idx1 = toId (Special ListCon)

a = TyVar (Ident "a")
a0 = TyVar (Ident "$0")

tla = TyList a
tla' = TyList a0

-- test 3 : literals

test3 = (li, ([], tChar))

li = Lit (Char 'a')

-- test 4 : infix applications

test4 = (infixapp, ([], TyList tChar))

cons = Special Cons
idcons = toId cons
tcons = TyFun a (TyFun tla tla)
infixapp = InfixApp li (QConOp cons) x1

-- test 5 : infix applications

test5 = (infixapp2, ([numconstr a0, numconstr a0, numconstr a0], a0))

infixapp2 = InfixApp l1 plus l1
l1 = Lit (Int 1)
plus = QVarOp (UnQual (Symbol "+"))
idplus = toId plus
tplus = TyFun a (TyFun a a)
ctrplus = numconstr a

-- test 6 : applications

test6 = (app1, ([numconstr a0, numconstr a0], a0))

f = Var (UnQual (Ident "f"))
idf = toId (UnQual (Ident "f"))
app1 = App f l1
tf = TyFun a a
ctrf = numconstr a

-- test 7 : negated expressions

test7 = (ne, ([numconstr a0, numconstr a0], a0))

ne = NegApp l1

-- test 8 : lambda abstraction

test8 = (ab, ([], TyFun a0 a0))
ab = Lambda (SrcLoc "" 0 0) [PVar (Ident "x")]
            (Var (UnQual (Ident "x")))

-- test 9 : if expressions

test9 = (ife, ([], tBool))

ife = If true true false

true = Con (UnQual (Ident "True"))
false = Con (UnQual (Ident "False"))

itrue = toId (UnQual (Ident "True"))
ifalse = toId (UnQual (Ident "False"))

-- test 10: case expressions

test10 = (ecase, ([numconstr a0, numconstr a0, numconstr a0], a0))

ecase = Case exs [alt1, alt2]

exs = Var (UnQual (Ident "xs"))
ex = Var (UnQual (Ident "x"))

ix = toId (UnQual (Ident "x"))
ixs = toId (UnQual (Ident "xs"))

a1 = TyVar (Ident "$100")

loc = SrcLoc "" 0 0

alt1 = Alt loc px galt1 (BDecls [])
alt2 = Alt loc PWildCard galt2 (BDecls [])

galt1 = UnGuardedAlt (InfixApp ex plus l1)
galt2 = UnGuardedAlt (Lit (Int 0))

px = PInfixApp (PVar (Ident "x")) cons PWildCard

-- test 11: do expressions

test11 = (Do [stmt1, stmt2], ([showconstr a1], tio unit))

stmt1 = Generator loc s (App ere efi)
stmt2 = Qualifier (App epr es)

s = PVar (Ident "s")

es = Var (UnQual (Ident "s"))
efi = Var (UnQual (Ident "file"))
ere = Var (UnQual (Ident "readFile"))
epr = Var (UnQual (Ident "print"))

idfi = toId (UnQual (Ident "file"))
idre = toId (UnQual (Ident "readFile"))
idpr = toId (UnQual (Ident "print"))

tio t = TyApp (TyCon (UnQual (Ident "IO"))) t

unit = TyCon (Special UnitCon)

tre = TyFun (tList tChar) (tio (tList tChar))
tpr = TyFun a (tio unit)

showconstr t = ClassA (UnQual (Ident "Show")) [t]

idshow = toId (UnQual (Ident "Show"))
idmonad = toId (UnQual (Ident "Monad"))

showc = Class idshow [a] [] [] [ishows]
ishows = Inst idshow [tList tChar] []

monadc = Class idmonad [a] [] [] [imonadio]

imonadio = Inst idmonad [TyCon (UnQual (Ident "IO"))] []

-- test 12 : tuple expressions

test12 = (Tuple Boxed [li, l1], ([numconstr a0], TyTuple Boxed [tChar, a0]))

-- test 13 : list expressions

test13 = (List [l1], ([numconstr a0], TyList a0))

-- test 14 : left section

test14 = (LeftSection l1 plus, ([numconstr a0, numconstr a0], TyFun a0 a0))

-- test 15 : left section

test15 = (RightSection plus l1, ([numconstr a0, numconstr a0], TyFun a0 a0))

-- test 16 : record construction

test16 = (RecConstr rqname [fup1, fup2], ([], TyCon rqname))

rname = Ident "Person"
rqname = UnQual rname
rid = toId rqname

qnamename = UnQual (Ident "name")
idnamename = toId qnamename
qnameage = UnQual (Ident "age")
idnameage = toId qnameage

lname = Lit (String "a")

fup1 = FieldUpdate qnamename lname
fup2 = FieldUpdate qnameage l1

-- test 17 : record update

test17 = (RecUpdate (RecConstr rqname [fup1, fup2]) [fup3], ([], TyCon rqname))

fup3 = FieldUpdate qnameage (Lit (Int 2))

-- test 18 : enumeration from

test18 = (EnumFrom l1, ([enumconstr a0, numconstr a0], TyList a0))

-- test 19 : enumeration from to

test19 = (EnumFromTo l1 (Lit (Int 2)), ([enumconstr a0, enumconstr a0, numconstr a0, numconstr a0], TyList a0))

-- test 20 : enumeration from then

test20 = (EnumFromThen l1 (Lit (Int 2)), ([enumconstr a0, enumconstr a0, numconstr a0, numconstr a0], TyList a0))

-- test 21 : enumeration from then to

test21 = (EnumFromThenTo l1 (Lit (Int 2)) (Lit (Int 5)),
               ([enumconstr a0, enumconstr a0, enumconstr a0, numconstr a0, numconstr a0, numconstr a0],TyList a0))

-- test 22 : list comprehensions

test22 = (ListComp xv [gen1, qual1], ([enumconstr a0, numconstr a0, intrconstr a0], TyList a0))

evenid = toId (UnQual (Ident "even"))
evenv = Var (UnQual (Ident "even"))
xv = Var (UnQual (Ident "x"))
gen1 = QualStmt (Generator loc (PVar (Ident "x")) (EnumFrom l1))
qual1 = QualStmt (Qualifier (App evenv xv))

intrconstr t = ClassA (UnQual (Ident "Integral")) [t]

inti = toId (UnQual (Ident "Integral"))

intidc = Class (inti) [a] [] [] [intint]

intint = Inst (toId (UnQual (Ident "Integral"))) [tInt] []

numi = toId (UnQual (Ident "Num"))

numidc = Class (inti) [a] [] [] [numint]

numint = Inst (toId (UnQual (Ident "Num"))) [tInt] []

enumi = toId (UnQual (Ident "Enum"))

enumidc = Class (inti) [a] [] [] [enumint]

enumint = Inst (toId (UnQual (Ident "Enum"))) [tInt] []

-- test 23 : expressions with type signature

test23 = (ExpTypeSig loc e (TyForall Nothing [numconstr a0] a0), ([numconstr a0, numconstr a0], a0))

e = Lit (Int 1)

-- test 24 : let expressions

test24 = (Let bs et24, ([numconstr a0], a0))

idf' = Ident "f"

px' = Ident "x"
et24 = App (Var (UnQual idf')) e
bs = BDecls [fb]
fb = FunBind [m]
m = Match loc idf' [PVar px'] Nothing (UnGuardedRhs (Var (UnQual px'))) (BDecls [])

-- environments for the tests

gamtest = singleton $ Env.fromList [(idx, idx :>: TyForall Nothing [] tInt),
                        (idx1, idx1 :>: TyForall Nothing [] tla),
                        (idcons, idcons :>: TyForall Nothing [] tcons),
                        (idplus, idplus :>: TyForall Nothing [ctrplus] tplus),
                        (idf, idf :>: TyForall Nothing [ctrf] tf),
                        (itrue, itrue :>: TyForall Nothing [] tBool),
                        (ifalse, ifalse :>: TyForall Nothing [] tBool),
                        (ixs, ixs :>: TyForall Nothing [] a1),
                        (idfi, idfi :>: TyForall Nothing [] (tList tChar)),
                        (idre, idre :>: TyForall Nothing [] tre),
                        (idpr, idpr :>: TyForall Nothing [showconstr a] tpr),
                        (rid, rid :>: TyForall Nothing [] (TyFun (tList tChar) (TyFun tInt (TyCon rqname)))),
                        (evenid, evenid :>: TyForall Nothing [integralconstr a] (TyFun a tBool))]
clstest = Env.fromList [(idshow, showc), (idmonad, monadc), (inti, intidc), (numi, numidc), (enumi, enumidc)]
lblenv = Env.fromList [(rid, [(idnamename, tList tChar), (idnameage, tInt)])]
