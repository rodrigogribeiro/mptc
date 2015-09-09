
module Tests.Cases.TypeSynExpandTest where

import Language.Haskell.Exts

import Utils.ExpandTySyn

testtysynexpand1 = testtysynexpand test1

testtysynexpand (f,s)
    = do
        r <- parseFileWithExts exts f
        let
           r' = parseResult id undefined r
        r'' <- expandSynsInModule [] r'
        --putStrLn (prettyPrint $ snd r'')
        return ()

test1 = ("/home/rodrigo/Dropbox/projects/haskell/mptc/src/Tests/Data/TestCase1TypeSynExpand.hs", "")

-- case analysis over the parser result

parseResult :: (a -> b) -> (SrcLoc -> String -> b) -> ParseResult a -> b
parseResult f g (ParseOk m) = f m
parseResult f g (ParseFailed l s) = g l s

-- always enabled extensions

exts :: [Extension]
exts = map EnableExtension [MultiParamTypeClasses, RankNTypes, FlexibleContexts]
