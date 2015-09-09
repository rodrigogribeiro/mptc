
module Tests.Cases.TcPatTest where

import Test.HUnit

import Tc.TcPat
import Tc.TcEnv

import Tests.Data.TestCase1TcPat
import Utils.EnvMonad
import qualified Utils.Env as Env
import Utils.Stack

testtcpat1 = testtcpat test1
testtcpat2 = testtcpat test2
testtcpat3 = testtcpat test3
testtcpat4 = testtcpat test4
testtcpat5 = testtcpat test5
testtcpat6 = testtcpat test6
testtcpat7 = testtcpat test7
testtcpat8 = testtcpat test8
testtcpat9 = testtcpat test9
testtcpat10 = testtcpat test10
testtcpat11 = testtcpat test11

testtcpat (e,r) 
    = do
        (r',_) <- run genEnv (tcPat e)
        assertEqual "" (Right r) r'

gamtest = singleton (Env.fromList [cons, node, foo, fooc])
clstest = Env.empty

genEnv = TcEnv clstest gamtest lblenv emptyStack 0 []
