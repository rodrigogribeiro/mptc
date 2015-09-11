
module Cases.TestWellFormedTypes where

import Data.Maybe
import Test.HUnit
import Language.Haskell.Exts

import Tc.TcEnv
import Tc.TcWellformed
import Data.TestCase1WellFormedTypes
import Utils.EnvMonad
import qualified Utils.Env as Env
import Utils.Stack

testwellformedty1 = testwellformedty test1
testwellformedty2 = testwellformedty test2

testwellformedty (ty,cls, r) 
    = do
        (r',_) <- run (genEnv cls) (wf ty [])
        --print r'
        assertEqual "" r (either (const False) isJust r')

genEnv cls = TcEnv cls gamtest Env.empty emptyStack 0 []

gamtest = singleton $ Env.fromList []
