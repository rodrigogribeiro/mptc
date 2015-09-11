
module Data.TestCase1WellFormedInstances where

import Language.Haskell.Exts

import Tc.Class
import Tc.TcMonad

import Utils.Id
import Utils.Env

test1 = (inst1, True)
test2 = (inst2, True)

aid = toId (UnQual (Ident "A"))
bid = toId (UnQual (Ident "B"))
cid = toId (UnQual (Ident "C"))
did = toId (UnQual (Ident "D"))

tva = TyVar (Ident "$a")
tvb = TyVar (Ident "$b")
tvc = TyVar (Ident "$c")

tint = TyCon (UnQual (Ident "Int"))

cls1 = Class aid [tva] [] [] [inst1]
inst1 = Inst aid [tint] []

cls2 = Class bid [tva, tvb] [] [] []

cls3 = Class cid [tva] [] [] []

cls4 = Class did [tva, tvb, tvc] [asst1, asst2] [] [inst2]

asst1 = ClassA (UnQual (Ident "A")) [tva]
asst2 = ClassA (UnQual (Ident "B")) [tva, tvb]

inst2 = Inst did [tint, tvb, tvc] [asst3, asst4]
asst3 = ClassA (UnQual (Ident "B")) [tint, tvb]
asst4 = ClassA (UnQual (Ident "C")) [tvc]


cls = fromList [(aid, cls1), (bid, cls2), (cid,cls3), (did, cls4)]
