

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Cases.DeclDependencyTest
import Cases.KindDependencyTest
import Cases.KindInferenceTest
import Cases.SatTest
import Cases.TcExpTest
import Cases.TcLiteralTest
import Cases.TcPatTest
import Cases.TestWellFormedTypes
import Cases.TypeSynExpandTest
import Cases.TcLcgTest
import Cases.TypeOrderingTest
import Cases.InstDerivingTest
import Cases.DataConsInfoTest
import Cases.ClassInstCollectorTest
import Cases.IfaceWriterTest
import Cases.IfaceReaderTest

main = defaultMain tests

tests = [
            testGroup "Dependency Analysis Tests" 
                [testCase "testkinddependency1" testkinddependency1,
                 testCase "testkinddependency2" testkinddependency2,
                 testCase "testdecldependency1" testdecldependency1],
            testGroup "Kind Inference Tests"
                [testCase "testkindinference1" testkindinference1],
            testGroup "Type Synonym Expansion"
                [testCase "testtypesynexpand1" testtysynexpand1],
            testGroup "Type Ordering Tests"
                [testCase "testordering1" testordering1,
                 testCase "testordering2" testordering2,
                 testCase "testordering3" testordering3,
                 testCase "testordering4" testordering4,
                 testCase "testordering5" testordering5,
                 testCase "testordering6" testordering6,
                 testCase "testordering7" testordering7,
                 testCase "testordering8" testordering8,
                 testCase "testordering9" testordering9,
                 testCase "testordering10" testordering10],
            testGroup "Satisfiability Tests"
                [testCase "testsat1" testsat1,
                 testCase "testsat2" testsat2,
                 testCase "testsat3" testsat3,
                 testCase "testsat4" testsat4,
                 testCase "testsat5" testsat5,
                 testCase "testsat6" testsat6,
				 testCase "testsat7" testsat7,
				 testCase "testsat8" testsat8,
				 testCase "testsat9" testsat9,
				 testCase "testsat10" testsat10,
				 testCase "testsat11" testsat11],
            testGroup "Well formedness Tests"
               [testGroup "Well formed types tests"
                  [testCase "testwellformedty1" testwellformedty1,
                   testCase "testwellformedty2" testwellformedty2]
                ],
            testGroup "Lcg Tests" 
                [testCase "testlcg1" testlcg1],
            testGroup "Type Inference Tests"
                 [testGroup "Type Inference for Literals Tests" 
                    [testCase "testtclit1" testtclit1,
                     testCase "testtclit2" testtclit2,
                     testCase "testtclit3" testtclit3,
                     testCase "testtclit4" testtclit4],
                  testGroup "Type Inference for Patterns Tests"
                     [testCase "testtcpat1" testtcpat1,
                      testCase "testtcpat2" testtcpat2,
                      testCase "testtcpat3" testtcpat3,
                      testCase "testtcpat4" testtcpat4,
                      testCase "testtcpat5" testtcpat5,
                      testCase "testtcpat6" testtcpat6,
                      testCase "testtcpat7" testtcpat7,
                      testCase "testtcpat8" testtcpat8,
                      testCase "testtcpat9" testtcpat9,
                      testCase "testtcpat10" testtcpat10,
                      testCase "testtcpat11" testtcpat11 ],
                  testGroup "Type Inference for Expressions Tests" 
                     [testCase "testtcexp1" testtcexp1, 
                      testCase "testtcexp2" testtcexp2,
                      testCase "testtcexp3" testtcexp3,
                      testCase "testtcexp4" testtcexp4,
                      testCase "testtcexp5" testtcexp5,
                      testCase "testtcexp6" testtcexp6,
                      testCase "testtcexp7" testtcexp7,
                      testCase "testtcexp8" testtcexp8,
                      testCase "testtcexp9" testtcexp9,
                      testCase "testtcexp10" testtcexp10,
                      testCase "testtcexp11" testtcexp11,
                      testCase "testtcexp12" testtcexp12,
                      testCase "testtcexp13" testtcexp13,
                      testCase "testtcexp14" testtcexp14,
                      testCase "testtcexp15" testtcexp15,
                      testCase "testtcexp16" testtcexp16,
                      testCase "testtcexp17" testtcexp17,
                      testCase "testtcexp18" testtcexp18,
                      testCase "testtcexp19" testtcexp19,
                      testCase "testtcexp20" testtcexp20,
                      testCase "testtcexp21" testtcexp21,
                      testCase "testtcexp22" testtcexp22,
                      testCase "testtcexp23" testtcexp23,
                      testCase "testtcexp24" testtcexp24]],
            testGroup "Interface Related Tests" 
                [testGroup "Instance Deriving for Data Types Tests" 
                    [testCase "testinstderiving1" testinstderiving1],
                 testGroup "Assumptions for Data Types"
                    [testCase "testdataconsinfo1" testdataconsinfo1],
                 testGroup "Class and Instances Info"
                    [testCase "testclassinstcollect1" testclassinstcollect1]
                ]                                  
        ]
