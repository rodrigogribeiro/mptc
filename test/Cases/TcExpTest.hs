
module Cases.TcExpTest where

import Language.Haskell.Exts
import Test.HUnit

import Tc.TcAlphaEq
import Tc.TcExp
import Tc.TcEnv

import Data.TestCase1TcExp
import Utils.EnvMonad
import qualified Utils.Env as Env
import Utils.Stack

testtcexp1 = testtcexp test1
testtcexp2 = testtcexp test2
testtcexp3 = testtcexp test3
testtcexp4 = testtcexp test4
testtcexp5 = testtcexp test5
testtcexp6 = testtcexp test6
testtcexp7 = testtcexp test7
testtcexp8 = testtcexp test8
testtcexp9 = testtcexp test9
testtcexp10 = testtcexp test10
testtcexp11 = testtcexp test11
testtcexp12 = testtcexp test12
testtcexp13 = testtcexp test13
testtcexp14 = testtcexp test14
testtcexp15 = testtcexp test15
testtcexp16 = testtcexp test16
testtcexp17 = testtcexp test17
testtcexp18 = testtcexp test18
testtcexp19 = testtcexp test19
testtcexp20 = testtcexp test20
testtcexp21 = testtcexp test21
testtcexp22 = testtcexp test22
testtcexp23 = testtcexp test23
testtcexp24 = testtcexp test24

testtcexp (e,r@(ctx,t)) 
    = do
        (r',_) <- run genEnv (tcExp e)
        --either print (putStrLn . show) r'
        assertBool "" (isRight r')   
        let r'' = fromRight r' 
        assertBool "" ((alphaEq ctx (fst r'')) && (alphaEq t (snd r'')))
        
genEnv = TcEnv clstest gamtest lblenv emptyStack 0 []    

isRight (Right _) = True
isRight _ = False    

fromRight (Right x) = x
