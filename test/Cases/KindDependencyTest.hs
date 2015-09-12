module Cases.KindDependencyTest where

import Cases.BaseDir
    
import Language.Haskell.Exts hiding (name)
import Test.HUnit

import Utils.DependencyAnalysis
import Utils.Nameable

testkinddependency1 = testkinddependency test1
testkinddependency2 = testkinddependency test2

testkinddependency :: (String, String) -> Assertion
testkinddependency (f,s)
    = do
        r <- parseFileWithExts exts f
        let
           r' = parseResult decls undefined r
           ns = show $ map (map name) (dependencyAnalysis KindAnalysis r')
        assertEqual "Kind Dependency Result:" s ns

test1 = (baseDir ++ "/mptc/test/Data/TestCase1KindDependence.hs", "[[T,C]]")
test2 = (baseDir ++ "/mptc/test/Data/TestCase2KindDependence.hs", "[[GRose],[LNode]]")

decls (Module _ _ _ _ _ _ ds) = ds
    -- pattern matching function over parser results

parseResult :: (a -> b) -> (SrcLoc -> String -> b) -> ParseResult a -> b
parseResult f g (ParseOk m) = f m
parseResult f g (ParseFailed l s) = g l s

-- always enabled extensions

exts :: [Extension]
exts = map EnableExtension [MultiParamTypeClasses, RankNTypes, FlexibleContexts]
