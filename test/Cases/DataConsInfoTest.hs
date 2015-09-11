
module Cases.DataConsInfoTest where

import Data.List (sort)
import Language.Haskell.Exts hiding (name)
import Test.HUnit 

import Iface.DataConsCollector
import Tc.Assumption
import Tc.Class
import Tc.TcMonad
import Tc.TcEnv

import qualified Utils.Env as Env
import Utils.Stack
import Utils.Id
import Utils.EnvMonad


testdataconsinfo1 = testdataconsinfo file

testdataconsinfo f 
    = do
        m <- parseFile f
        case m of
            ParseOk m' -> gen m'
            _          -> error "Parse error"
            
gen m = do
          (as,_) <- run (genEnv initcls initgam) (collectDataConsInfo m)
          as' <- either (\_ -> error "error...") return as
          --print as'
          assertEqual "" (sort assumps) (sort $ (fst3 as') ++ (snd3 as'))
          assertEqual "" (sort [(ip,[lbl1,lbl2])]) (sort (trd3 as'))
          
assumps = [nila, consa, persona, reccona, namea, agea]                       

-- assumptions

nila = toId (Ident "Nil") :>: (TyForall Nothing [] tml)
consa = toId (Ident "Cons") :>: (TyForall Nothing [] (tva +-> (tml +-> tml))) 

persona = toId (Ident "Person") :>: (TyForall Nothing [] (ts +-> (ti +-> tr)))
reccona = toId (Ident "RecCon") :>: (TyForall Nothing [] (ts +-> (ti +-> tr)))

namea = toId (Ident "name") :>: TyForall Nothing [] (tr +-> ts)
agea = toId (Ident "age") :>: TyForall Nothing [] (tr +-> ti)

-- labels

ip = toId (Ident "Person") 

lbl1 = (toId (Ident "name"), ts)
lbl2 = (toId (Ident "age"),ti)

tml = (TyApp (TyCon (UnQual (Ident "MyList"))) tva)
tr = TyCon (UnQual (Ident "Rec"))
ts = TyCon (UnQual (Ident "String"))
ti = TyCon (UnQual (Ident "Int"))

l +-> r = TyFun l r


genEnv cls gam = TcEnv cls gam Env.empty emptyStack 0 []


initcls = Env.fromList []
initgam = singleton (Env.empty)

tva = TyVar (Ident "a")

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

-- test file

file = "/home/rodrigo/Dropbox/projects/haskell/mptc/src/Tests/Data/Full/Teste1.hs"

