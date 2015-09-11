
module Cases.TypeOrderingTest where

import Control.Monad.Trans (liftIO)

import Test.HUnit

import Tc.TcOrdering

import Data.TestCase1TypeOrdering
import Utils.EnvMonad


testordering1 = testordering testcase1

testordering2 = testordering testcase2

testordering3 = testordering testcase3

testordering4 = testordering testcase4

testordering5 = testordering testcase5

testordering6 = testordering testcase6

testordering7 = testordering testcase7

testordering8 = testordering testcase8

testordering9 = testordering testcase9

testordering10 = testordering testcase10

testordering (t,t',r) = testordering' (t,t',r) testenv >> return ()
                        

testordering' (t,t',r) e  
    = run e $ do
                r' <- t <=: t'
                s' <- t >: t'
                return (r == r' && r /= s')
