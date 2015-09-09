
module Tests.Cases.SatTest where

import Test.HUnit

import Tc.TcSat
import Tc.TcEnv

import Tests.Data.TestCase1Sat
import Utils.EnvMonad
import qualified Utils.Env as Env
import Utils.Stack
 
testsat1 = testsat' test1
testsat2 = testsat' test2
testsat3 = testsat' test3
testsat4 = testsat' test4
testsat5 = testsat' test5
testsat6 = testsat' test6
testsat7 = testsat' test7
testsat8 = testsat' test8
testsat9 = testsat' test9
testsat10 = testsat' test10
testsat11 = testsat' test11

testsat' (cls,gam,k,r) 
    = do
        (r', _) <- run (genEnv cls gam) (sat k)
        --either putStrLn print r'
        assertEqual "" r (isRight r')          

-- generating an environment for test

genEnv cls gam = TcEnv cls gam Env.empty emptyStack 0 []

-- testing a right value

isRight (Right _) = True
isRight _         = False
