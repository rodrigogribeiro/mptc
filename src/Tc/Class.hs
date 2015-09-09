
module Tc.Class (Class (..), Inst (..)) where

import Language.Haskell.Exts hiding (name)

import Tc.Assumption
import Utils.Id

-- this module defines a data type for
-- representing class and instances
-- to be used in type inference

data Class = Class {
                name :: Id,
                parameters :: [Type], -- INVARIANT: must be a type variable
                supers :: [Asst], 
                members :: [Assumption],
                instances :: [Inst]
             } deriving (Eq, Ord)
             
data Inst = Inst {
                instname :: Id,
                instparameters :: [Type],
                instsupers :: [Asst]
            } deriving (Eq, Ord)           

            
instance Show Class where
    show c = "class" <+> showconstr (supers c)
                     <+> show (name c)
                     <+> show' (parameters c)
                     <+> "where"
                     <+> braces (hsep (members c))
    
instance Show Inst where
    show i = "instance" <+> showconstr (instsupers i)
                        <+> show (instname i)
                        <+> show' (instparameters i)
    
showconstr [] = ""
showconstr xs = parens (showcons xs) <+> "=>"
                where
                    showcons [x] = pprasst x
                    showcons (x:xs) = pprasst x ++ "," <+> showcons xs

pprasst :: Asst -> String                    
pprasst = prettyPrint
 
braces :: String -> String   
braces x = "{" $+$ x $+$ "}"    

parens :: String -> String
parens x = "(" ++ x ++ ")"

(<+>) :: String -> String -> String
x <+> y = x ++ " " ++ y  

($+$) :: String -> String -> String
x $+$ y = x ++ "\n" ++ y

show' :: Pretty a => [a] -> String
show' = foldr ((<+>) . prettyPrint) []

hcat :: Pretty a => [a] -> String
hcat = foldr ((<+>) . prettyPrint) []

hsep :: Show a => [a] -> String
hsep = foldr (($+$) . ((++ ";") . show)) []
