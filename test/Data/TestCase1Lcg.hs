
module Data.TestCase1Lcg where

import Language.Haskell.Exts

import Tc.TcEnv
import qualified Utils.Env as Env
import Utils.Stack
import Utils.Id


test1 = (toId (UnQual (Ident ("map"))), [ty1,ty2], trf)

ty1 = (tva +-> tvb) +-> (tla +-> tlb)
ty2 = (tva +-> tvb) +-> (tca +-> tcb)

tc t = TyApp (TyCon (UnQual (Ident "Tree"))) t

tca = tc tva
tcb = tc tvb

tla = TyList tva
tlb = TyList tvb

tva = TyVar (Ident "a")
tvb = TyVar (Ident "b")
tvc = TyVar (Ident "c")

tv0 = TyVar (Ident "$0")
tv1 = TyVar (Ident "$1")
tv2 = TyVar (Ident "$2")
tv3 = TyVar (Ident "$3")
tv4 = TyVar (Ident "$4")

trf = TyForall Nothing [c] tr

c = ClassA (UnQual (Ident "Map")) [tv4]

tr = (tv0 +-> tv1) +-> ((TyApp tv4 tv2) +-> (TyApp tv4 tv3))

(+->) :: Type -> Type -> Type
l +-> r = TyFun l r
