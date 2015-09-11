
module Cases.ClassInstCollectorTest where

import Language.Haskell.Exts

import Iface.ClassInstCollector

import Tc.Assumption
import Tc.TcMonad
import Tc.TcEnv
import Tc.Class

import Utils.Id
import Utils.EnvMonad
import Utils.Stack
import qualified Utils.Env as Env

import Test.HUnit

testclassinstcollect1 = testclassinstcollect file

testclassinstcollect f 
    = do
        m <- parseFile f
        case m of
            ParseOk m' -> gen m'
            _          -> error "Parse error"
            
gen m = do
          as <- run (genEnv initcls initgam) (collectClassInstData Env.empty m)
          (cls,is,bs) <- either (\_ -> error "error...") return (fst as)
          --mapM (putStrLn . prettyPrint ) bs
          assertEqual "" cls [clsmeq]
          assertEqual "" is [instmeq]

clsmeq = Class meqid [tva,tvb] [] [eqass] [] 
instmeq = Inst meqid [tint,tfloat] [] 

meqid = toId (Ident "MyEq")
eqid = toId eqs
eqt = tva +-> (tvb +-> tbool)
eqass = eqid :>: TyForall Nothing [] eqt

eqs = Symbol "=:="

l +-> r = TyFun l r


genEnv cls gam = TcEnv cls gam Env.empty emptyStack 0 []


initcls = Env.fromList []
initgam = singleton (Env.empty)

tva = TyVar (Ident "a")
tvb = TyVar (Ident "b")
tbool = TyCon (UnQual (Ident "Bool"))
tint = TyCon (UnQual (Ident "Int"))
tfloat = TyCon (UnQual (Ident "Float"))

-- test file

file = "/home/rodrigo/Dropbox/projects/haskell/mptc/src/Tests/Data/Full/Teste1.hs"
