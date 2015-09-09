
module Tests.Cases.TcLcgTest where

import Test.HUnit

import Tc.TcLcg
import Tc.TcEnv

import Tests.Data.TestCase1Lcg
import Utils.EnvMonad
import qualified Utils.Env as Env
import Utils.Stack
import Language.Haskell.Exts

testlcg1 = testtclcg test1


testtclcg (i,ts,t) 
    = do
        (r',_) <- run genEnv (lcg i ts)
        assertEqual "" (Right t) r'

gamtest = singleton $ Env.fromList []
clstest = Env.empty
lblenv = Env.empty

genEnv = TcEnv clstest gamtest lblenv emptyStack 0 []