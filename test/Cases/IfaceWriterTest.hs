
module Tests.Cases.IfaceWriterTest where

import Data.List
import qualified Data.Map as Map

import Language.Haskell.Exts hiding (name)

import Tc.Assumption
import Tc.Class
import Tc.Kc.KcEnv

import Iface.IfaceWriter

import Utils.Id
import qualified Utils.Env as Env

testifacewriter1 
    = do
        writeInterface name1 m (toId (Ident "Teste1")) [syn1, syn2] (Map.fromList [lbl]) kcenv classes insts varenv

name1 = "/home/rodrigo/Dropbox/projects/haskell/mptc/src/Tests/Data/Full/"

m = ModuleName "Teste1"        
        
kcenv = insertTypeKind (toId (Ident "MyList")) (KindFn KindStar KindStar)
          (insertClassKind myeqid myeqkind emptyEnv)  
          
varenv =  [nilassump, consassump, eqassump]  
                       
classes = [myeqconstr]

insts = [myeqinstconstr, insteqmylist, instordmylist]                                    

-- definition for data type MyList

tcmylist = TyApp (TyCon (UnQual (Ident "MyList"))) tva
idnil = toId (Ident "Nil")
idcons = toId (Ident "Cons")

nilassump = idnil :>: (TyForall Nothing [] tcmylist)
consassump = idcons :>: (TyForall Nothing [] (tva +-> tcmylist +-> tcmylist))

ideq = toId (Ident "Eq")
idord = toId (Ident "Ord")

insteqmylist = Inst ideq [tcmylist] []
instordmylist = Inst idord [tcmylist] []

-- definition of some synonyms

syn1 = (TyCon (UnQual (Ident "String")), TyList (TyCon (UnQual (Ident ("Char")))))
syn2 = (TyApp (TyCon (UnQual (Ident "Vec"))) tva, TyList (TyCon (UnQual (Ident "Int"))))

-- definition of a label

nid = toId (Ident "name")
tn = TyForall Nothing [] tbool +-> tva

lbl = (toId (Ident "R"), [(nid, tn)])


-- definitions for class myeq

myeqid = toId (Ident "MyEq")
myeqkind = KindFn KindStar KindStar 
myeqconstr = Class myeqid [tva, tvb] [] [eqassump] []

tva = TyVar (Ident "a")
tvb = TyVar (Ident "b")

eqsym = Symbol "=:="

cme = ClassA (UnQual (Ident "MyEq")) [tva, tvb]

eqassump = (toId eqsym) :>: TyForall Nothing [cme] (tva +-> (tvb +-> tbool))

tbool = TyCon (UnQual (Ident "Bool"))

-- definitions for instance MyEq Int Float

myeqinstconstr = Inst myeqid [tint, tfloat] []

tint = TyCon (UnQual (Ident "Int"))
tfloat = TyCon (UnQual (Ident "Float"))

x +-> y = TyFun x y
