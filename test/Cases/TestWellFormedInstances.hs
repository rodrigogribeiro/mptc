
module Tests.Cases.TestWellFormedInstances where

import Data.Maybe
import Test.HUnit
import Language.Haskell.Exts

import Tc.TcEnv
import Tc.TcWellformed
import Tests.Data.TestCase1WellFormedInstances
import Utils.EnvMonad
import qualified Utils.Env as Env
import Utils.Stack


testwellformedinst1 = testwellformedinst test1
testwellformedinst2 = testwellformedinst test2

testwellformedinst (i,r) 
    = do
        (r', _) <- run genEnv (wf i [])
        assertEqual "" r (either (const False) isJust r')

genEnv = TcEnv cls gamtest Env.empty emptyStack 0 []

gamtest = singleton $ Env.fromList []

gam = Env.empty