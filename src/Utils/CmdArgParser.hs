
module Utils.CmdArgParser (Arguments (..), 
                           Arg (..),
                           getArguments) where

-- this module defines a command line
-- argument parser

import Control.Applicative hiding (many)
import Control.Monad.Identity
import Data.List
import System.Environment (getArgs)
import System.Posix.Files (fileExist)
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

-- simple data type for representing arguments

data Arguments = Arguments [Arg] FilePath
                 deriving (Eq, Ord)

-- Arguments for controling the behavior of the
-- algorithm

data Arg = KindInfo            -- represented by --k
         | ClassInfo           -- represented by --c
         | InstInfo            -- represented by --i
         | AssumpInfo          -- represented by --a
         | Recomp              -- represented by --r
         | Make                -- enable multi module compilation
         | Help                -- shows help message
         deriving (Eq, Ord, Enum)

-- sample show instance for arguments
         
instance Show Arg where
    show KindInfo   = "--k"
    show ClassInfo  = "--c"
    show InstInfo   = "--i"
    show AssumpInfo = "--a"         
    show Recomp     = "--r"
    show Make       = "--make"
    show Help       = "--h"

type Parser a = ParsecT String () Identity a
         

-- getting parameters

getArguments :: IO (Either String Arguments)
getArguments 
    = do
        args <- getArgs
        let r = splitArgs args
        analysis r

analysis :: (Maybe [String], Maybe String) -> IO (Either String Arguments)
analysis (Nothing, Nothing) 
    = return (Left "No input arguments.\nUsage: for information try --h option.")  
analysis (Just x, Nothing) 
    = if "--h" `elem` x then return (Left helpMessage)
        else return (Left "Incorrect usage. For more information try --h option.")          
analysis (Nothing, Just f)
    = do
        b <- fileExist f
        if b then return (Right (Arguments [] f))
          else return (Left (concat ["The file:\n", f, "\ndoesn't exist."]))
analysis (Just as, Just f)
    = finish as f
        
finish :: [String] -> String -> IO (Either String Arguments)
finish ss s 
    = do
        let ps = concat ss
            args = parser ps
        either (return . Left) (complete s) args
        
complete :: String -> [Arg] -> IO (Either String Arguments)
complete s args
    = do
        b <- fileExist s
        if b then return (Right (Arguments args s))
          else return (Left (concat ["The file:\n", s, "\ndoesn't exist."]))                     

parser :: String -> Either String [Arg]
parser s 
    = case parse pargs "" s of
           Left _ -> Left "Incorrect usage. For more information try --h option."
           Right as -> Right as  

-- parses very simple arguments

pargs :: Parser [Arg]     
pargs 
    = many (arg ps)
      where
        ps = map (\i -> (show i, i)) [KindInfo ..]
        arg = choice . map f  
        f (s,k) = const k <$> try (symbol s)    
        
-- lexer definition
        
lexer = P.makeTokenParser emptyDef
symbol = P.symbol lexer            

-- some auxiliar functions

splitArgs :: [String] -> (Maybe [String], Maybe String)
splitArgs ss
    = case partition (\s -> (take 2 s) == "--") ss of
              ([],[x]) -> (Nothing, Just x)
              (ps,[x]) -> (Just ps, Just x)
              (ps,[])  -> (Just ps, Nothing)
              _        -> (Nothing, Nothing)
            
helpMessage :: String
helpMessage = "Usage:\nmain [options] file\n\n" ++
              "Options:\n--k: Show infered kinds\n"++
              "--c: Show infered class constraints\n" ++
              "--i: Show infered instance constraints\n" ++
              "--a: Show infered types (include data constructors)\n" ++
              "--r: Turn off recompilation checker\n" ++
              "--make : Enable multi-module processing\n" ++ 
              "--h: Show this message\n"            
