
module Utils.FileNameUtils where

import Language.Haskell.Exts
import System.FilePath
import System.Posix.Files

-- this module defines some very simple functions
-- for dealing with file names

-- generate the interface file name

interfaceFileName :: FilePath -> String -> FilePath
interfaceFileName dir s
    = genFileName dir s ".hi"
    
sourceFileName :: FilePath -> String -> FilePath
sourceFileName dir s
    = genFileName dir s ".hs"  
    
-- checks if a interface or source file exist

checkInterfaceFile :: FilePath -> ModuleName -> IO Bool
checkInterfaceFile dir = fileExist . interfaceFileName dir . moduleString

checkSourceFile :: FilePath -> ModuleName -> IO Bool
checkSourceFile dir m 
    = do
        let n = (sourceFileName dir . moduleString) m
        b <- fileExist n
        return b      
      
    
genFileName :: FilePath -> String -> String -> FilePath
genFileName dir s ex = dir ++ (toPath s) ++ ex   

toPath :: String -> String
toPath 
    = foldr step []
      where
        step c ac = if c == '.' then pathSeparator : ac else c : ac 
        
moduleString :: ModuleName -> String
moduleString (ModuleName s) = s    

-- breaking a string using a separator

breakAt :: Char -> String -> [String]
breakAt c 
    = uncurry (:) . foldr step ([],[])
      where
        step c' (s,ac) 
            | c == c' = ([],s:ac)
            | otherwise = (c':s,ac)
                
