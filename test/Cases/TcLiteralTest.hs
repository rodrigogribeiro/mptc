
module Cases.TcLiteralTest where


import Test.HUnit

import Tc.TcLiteral
import Tc.TcEnv

import Data.TestCase1TcLiteral
import Utils.EnvMonad
import qualified Utils.Env as Env
import Utils.Stack

testtclit1 = testtclit test1
testtclit2 = testtclit test2
testtclit3 = testtclit test3
testtclit4 = testtclit test4

testtclit (e,r) 
    = do
        (r',_) <- run genEnv (tcLiteral e)
        assertEqual "" (Right r) r'

gamtest = singleton $ Env.fromList []
clstest = Env.empty

genEnv = TcEnv clstest gamtest Env.empty emptyStack 0 []
