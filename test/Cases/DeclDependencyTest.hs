
module Cases.DeclDependencyTest where

import Language.Haskell.Exts hiding (name)
import Test.HUnit

import Utils.DependencyAnalysis
import Utils.Nameable


testdecldependency1 = testdecldependency test1

testdecldependency :: (String, String) -> Assertion
testdecldependency (f,s)
    = do
        r <- parseFileWithExts exts f
        let
           r' = parseResult decls undefined r
           ns = show $ map (map name) (dependencyAnalysis TypeAnalysis r')
        assertEqual "Type Dependency Result:" s ns

test1 = ("/home/rodrigo/Dropbox/projects/haskell/mptc/src/Tests/Data/TestCase1DeclDependence.hs", "[[a],[b],[h,g,f],[c,d]]")

decls (Module _ _ _ _ _ _ ds) = ds
    -- pattern matching function over parser results

parseResult :: (a -> b) -> (SrcLoc -> String -> b) -> ParseResult a -> b
parseResult f g (ParseOk m) = f m
parseResult f g (ParseFailed l s) = g l s

-- always enabled extensions

exts :: [Extension]
exts = map EnableExtension [MultiParamTypeClasses, RankNTypes, FlexibleContexts]
