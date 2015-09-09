
module Iface.IfaceDriver where


-- this module works as a facade for
-- the other modules in this package.
-- it exports only the function to
-- write a module info and to read
-- a set of module infos.

-- it uses the import / export definitions
-- in a module to write / read and to
-- report error messages

import Data.List
import qualified Data.Map as Map

import Language.Haskell.Exts
import System.FilePath

import Iface.Iface
import Iface.IfaceReader
import Iface.IfaceWriter

import Tc.Assumption 
import Tc.TySyn
import Tc.Kc.KcEnv hiding (unionl)

import Utils.Env (unionl)
import Utils.FileNameUtils
import Utils.Id

-- read a list of Ifaces based on imports of a given module

readIfaces :: FilePath -> Module -> IO [Iface]
readIfaces dir (Module _ _ _ _ _ is _)
    = mapM (readIface dir) is
    
readIface :: FilePath -> ImportDecl -> IO Iface
readIface dir i 
    = do
        let v = gen dir (importModule i)
        parseInterface v    

-- write Iface file using the export list

writeIface :: FilePath -> Module -> [Iface] -> Iface -> IO ()
writeIface  dir (Module _ n _ _ Nothing _ _) _ 
            (Iface i syn lbls kc cls ins ass) 
                = writeInterface dir n i syn lbls kc cls ins ass
writeIface dir (Module _ n _ _ (Just es) _ _) ms
            (Iface i syn lbls kc cls ins ass) 
                = writeInterface dir n i syn lbls kc cls ins ass
                                        
     

-- some auxiliar functions

moduleName (Module _ n _ _ _ _ _) = n

gen dir (ModuleName s) 
    = interfaceFileName dir s
               
