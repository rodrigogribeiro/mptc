
module Tests.Cases.FullTypeInferenceTest where


import Language.Haskell.Exts
import Utils.ErrMsg

import Main

-- this module runs the full algorithm over a
-- specified file.


fulltitest1 = fulltitest test1
fulltitest2 = fulltitest test2
fulltitest3 = fulltitest test3


fulltitest f 
    = do
        r <- parseFileWithExts exts f
        parseResult startCompilation parserErrMsg r
        
test1 = "/home/rodrigo/git/doutorado/mptc/src/Libs/PreludeBuiltIn.hs"
test2 = "/home/rodrigo/git/doutorado/mptc/src/Libs/PreludeBase.hs"
test3 = "/home/rodrigo/git/doutorado/mptc/src/Libs/PreludeList.hs"        