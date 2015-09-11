
module Data.TestCase1WellFormedTypes where

import Language.Haskell.Exts

import Tc.Class
import Tc.TcMonad

import Utils.Id
import Utils.Env

test1 = (ty1, cls, True)   -- (F Int b, O Int) => Int
test2 = (ty2, cls', False) -- (F a Int, O Int) => a

fid = toId (UnQual (Ident "F"))
oid = toId (UnQual (Ident "O"))

tva = TyVar (Ident "$20")
tvb = TyVar (Ident "$10")
tint = TyCon (UnQual (Ident "Int"))
tchar = TyCon (UnQual (Ident "Char"))

tb = TyCon (UnQual (Ident "Bool"))

fcls = Class fid [tva, tvb] [] [] [inst1]
fcls' = Class fid [tva, tvb] [] [] [inst1, inst3]
ocls = Class oid [tva] [] [] [inst2]

inst1 = Inst fid [tint, tb] []    -- instance F Int Bool
inst3 = Inst fid [tint, tchar] [] -- instance F Int Char
 
inst2 = Inst oid [tint] []        -- instance O Int

cls = fromList [(fid, fcls), (oid, ocls)]
cls' = fromList [(fid, fcls'), (oid, ocls)]

k = [ClassA (unid fid) [tint, tvb], ClassA (unid oid) [tint]]
k' = [ClassA (unid fid) [tva, tint], ClassA (unid oid) [tint]]

ty1 = TyForall Nothing k tint
ty2 = TyForall Nothing k' tint
