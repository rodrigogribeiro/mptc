
module Cases.KindInferenceTest where

import Cases.BaseDir
    
import Language.Haskell.Exts
import Data.Map (toList)
import Test.HUnit

import Tc.Kc.KcDriver
import Tc.Kc.KcEnv

testkindinference1 = testkindinference test1

testkindinference (f,s)
    = do
        r <- parseFileWithExts exts f
        let
           r' = parseResult id undefined r
        kcDriver r' emptyEnv
        return ()
        --mapM_ (\(i,k) -> putStrLn $ (show i) ++ " - " ++ (prettyPrint k)) (toList e)

test1 = (baseDir ++ "/mptc/test/Data/TestCase1KindInference.hs", "")

-- case analysis over the parser result

parseResult :: (a -> b) -> (SrcLoc -> String -> b) -> ParseResult a -> b
parseResult f g (ParseOk m) = f m
parseResult f g (ParseFailed l s) = g l s

-- always enabled extensions

exts :: [Extension]
exts = map EnableExtension [MultiParamTypeClasses, RankNTypes, FlexibleContexts]
