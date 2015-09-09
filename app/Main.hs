module Main where

import Control.Monad
import Data.List
import Data.Char
import Language.Haskell.Exts
import System.FilePath.Posix(splitFileName, pathSeparator, normalise)
import System.Posix.Files
import System.Directory (getCurrentDirectory)

import BuiltIn.InitialKindEnv(initialKindEnv)

import Iface.IfaceDriver
import Iface.Iface

import Tc.Kc.KcDriver
import qualified Tc.Kc.KcEnv as K
import Tc.TcDriver

import Utils.CmdArgParser
import Utils.ErrMsg hiding (empty)
import Utils.Env
import Utils.ExpandTySyn
import Utils.RecompilationChecker
import Utils.FileNameUtils

main :: IO()
main = do
        args <- getArguments
        either putStrLn (processArguments []) args

processArguments :: [ModuleName] -> Arguments -> IO ()
processArguments cycles (Arguments args f)
    = do
        need <- compilationNeeded f
        if not need && (Recomp `notElem` args) then putStrLn "No need for compilation."
           else do
                   let (dir, _) = splitFileName f
                   r <- parseFileWithExts exts f
                   parseResult (startCompilation cycles (map toLower dir) args) parserErrMsg r


-- this function does the hard job of compile a single module

startCompilation :: [ModuleName] -> FilePath -> [Arg] -> Module -> IO ()
startCompilation cycles dir args m
    = do
        -- building the working directory  for compilation
        compiler_dir <- liftM (map toLower) getCurrentDirectory
        let dir' = if compiler_dir `isPrefixOf` dir then dir
                      else concat [compiler_dir, [pathSeparator], dir]
            dirn = genDirName dir' (moduleName m)
            make_enabled = Make `elem` args
        -- loading information in interfaces
        putStrLn ("Compiling " ++ prettyPrint (moduleName m) ++ " ...")
        ifaces <- loadIfaces args make_enabled cycles dirn m
        either putStrLn (continueCompilation dir' m args) ifaces

continueCompilation :: FilePath -> Module -> [Arg] -> [Iface] -> IO ()
continueCompilation dir' m args ifaces
    = do
        -- expanding synonyms
        let
            syns = foldr (union . synonyms) [] ifaces
        (syns', m') <- expandSynsInModule syns m
        -- doing kind inference
        let
            kce = foldr (K.unionl . kinds) initialKindEnv ifaces
        kenv <- kcDriver m kce
        -- starting type inference
        let
            -- imported classes
            cls = project classes ifaces
            -- imported instances
            ins = project instances ifaces
            -- imported assumptions
            assums = project assumps ifaces
            -- imported labels
            lbls = foldr (unionl . labels) empty ifaces
            opts = filter (`notElem` [Make, Help]) args
        miface <- tcDriver syns' lbls kenv cls ins assums m'
        -- writing module interface and printing final inference results
        writeIface dir' m' ifaces miface
        let result = foldr f miface ([KindInfo .. AssumpInfo] \\ opts)
            f KindInfo ac = ac{kinds = K.emptyEnv}
            f ClassInfo ac = ac{classes = []}
            f InstInfo ac = ac{instances = []}
            f AssumpInfo ac = ac{assumps = []}
        print result

-- Loading interfaces using import declarations.
-- if multi-module compilation is turned on,
-- this function does the job.

type IfaceM = IO (Either String [Iface])

-- here we just call loadIfaces' passing the current module
-- name in order to avoid cycles

loadIfaces :: [Arg] -> Bool -> [ModuleName] -> FilePath -> Module -> IfaceM
loadIfaces args multi cycles dir m
    = loadIfaces' args multi dir m (moduleName m : cycles)


loadIfaces' :: [Arg] -> Bool -> FilePath -> Module -> [ModuleName] -> IfaceM
loadIfaces' args multi dir m cycles
    = do
        -- step 1 : getting imports from current module
        let
            imps = importsFrom m
            impsn = map importModule imps
        if null impsn then return (Right [emptyIface])
           else
        -- step 2 : checking if imports forms a cycle
               if not (null (impsn `intersect` cycles)) then return (Left $ cyclicImportsError (impsn ++ cycles))
                 else
        -- step 3 : checking if interface file for the imports exists
                      do
                        (hi,ni) <- partitionM (checkInterfaceFile dir) impsn
                        if not (null ni || multi) then return (Left $ interfacesDontExistError ni)
                           else
                               do
        -- step 4 : checking if files without interfaces, exists
                                 bad_imps <- filterM (checkSourceFile dir) ni
                                 if null bad_imps && not (null ni) then return (Left $ filesDontExistError bad_imps)
                                    else
        -- step 5 : compiling the modules without interface
                                        do
                                          mapM_ (processArguments cycles . Arguments args . sourceFileName dir . moduleString) ni
                                          liftM Right (readIfaces dir m)

-- pattern matching function over parser results

parseResult :: (a -> b) -> (SrcLoc -> String -> b) -> ParseResult a -> b
parseResult f _ (ParseOk m) = f m
parseResult _ g (ParseFailed l s) = g l s

-- always enabled extensions

exts :: [Extension]
exts = map EnableExtension [MultiParamTypeClasses, RankNTypes, FlexibleContexts, EmptyDataDecls, FunctionalDependencies]

-- some auxiliar functions

genDirName :: FilePath -> ModuleName -> FilePath
genDirName f (ModuleName n)
    = let
        fs = breakAt '/' f
        ns = breakAt '.' n
        step s (d,r)
            | s == head r = (d,r)
            | otherwise   = (s:d,r)
        s = fst (foldr step ([],ns) fs)
      in intercalate "/" s

project :: (Iface -> [a]) -> [Iface] -> [a]
project f = foldr ((++) . f) []


importsFrom :: Module -> [ImportDecl]
importsFrom (Module _ _ _ _ _ is _) = is

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f xs
    = do
        xt <- filterM f xs
        xf <- filterM (\x -> do {b <- f x ; return (not b)}) xs
        return (xt,xf)
