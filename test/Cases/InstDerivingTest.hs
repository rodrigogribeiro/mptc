
module Cases.InstDerivingTest where

import Cases.BaseDir
    
import Data.List (sort)
import Language.Haskell.Exts hiding (name)
import Test.HUnit 

import Iface.InstDeriving
import Tc.Class
import Tc.TcMonad
import Tc.TcEnv

import qualified Utils.Env as Env
import Utils.Stack
import Utils.Id
import Utils.EnvMonad

testinstderiving1 = testinstancederiving $ baseDir ++ "/mptc/test/Data/Full/Teste1.hs"


testinstancederiving f 
    = do
        m <- parseFile f
        case m of
            ParseOk m' -> gen m'
            _          -> error "Parse error"  

gen m' 
    = do
        is <- run (genEnv initcls initgam) (deriveInsts initcls m')
        is' <- either (\_ -> error "error...") return (fst is)
        assertEqual "" (sort is') (sort insts)
        

genEnv cls gam = TcEnv cls gam Env.empty emptyStack 0 []


initcls = Env.fromList [(ordid,ordc), (eqid, eqc)]
initgam = singleton (Env.empty)

-- instances generated

insts = [instl1, instr1, instl2]

instl1 = Inst eqid [tl] []
instl2 = Inst eqid [tr] []
instr1 = Inst ordid [tl] []

tl = TyApp (TyCon (UnQual (Ident "MyList"))) tva
tr = TyCon (UnQual (Ident "Rec")) 

-- initial type classes

tva = TyVar (Ident "a")

eqid = toId (Ident "Eq")
eqc = Class eqid [tva] [] [] []


ordid = toId (Ident "Ord")
ordc = Class ordid [tva] supord [] []

supord = [ClassA (UnQual (Ident "Eq")) [tva]]
