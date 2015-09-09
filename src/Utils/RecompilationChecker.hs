
module Utils.RecompilationChecker where


-- this module implements functions 
-- for checking if a module needs to
-- be compiled or not. 

-- this is done checking the timestamp
-- of the module's interface (if it exists)

import Control.Monad
import System.Posix.Files 
import System.FilePath

compilationNeeded :: FilePath -> IO Bool
compilationNeeded hsfile
    = do
        let
            (no,_) = splitExtension hsfile
            hifile = addExtension no ".hi"
        exist <- fileExist hifile
        if exist then checkTimeStamp hifile hsfile
          else return True

-- checking the timestamp of a interface file

checkTimeStamp :: FilePath -> FilePath -> IO Bool
checkTimeStamp hi hs
    = do
        let changeTime = liftM modificationTime . getFileStatus
        timeHs <- changeTime hs
        timeHi <- changeTime hi
        return (timeHi < timeHs)        