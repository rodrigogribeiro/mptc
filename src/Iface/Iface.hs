
module Iface.Iface (Iface(..), emptyIface)where

import qualified Data.Map as Map
import Language.Haskell.Exts hiding (name)
import Text.PrettyPrint.HughesPJ

import Tc.Kc.KcEnv
import Tc.Class
import Tc.Assumption
import Tc.TySyn
import Tc.TcLabel

import Utils.Id

-- this module only defines
-- the data type for interface
-- files


data Iface = Iface {
               ifacename :: Id,
               synonyms :: [TySyn],
               labels :: LabelEnv,
               kinds :: KcEnv,
               classes :: [Class],
               instances :: [Inst],
               assumps :: [Assumption]
             } deriving (Eq, Ord)
             
-- a constant for an empty iface

emptyIface :: Iface
emptyIface = Iface (toId (Ident "Main")) [] Map.empty Map.empty [] [] []             
             
-- show instance used for printing the
-- full iface result for a module

instance Show Iface where
    show = render . writeIface

writeIface :: Iface -> Doc
writeIface (Iface _ _ _ kc c i a)
    = scat [writeKindInfo kc, writeClassInfo c, 
            writeInstInfo i, writeAssumps a]

writeKindInfo :: KcEnv -> Doc
writeKindInfo e
    | Map.null e = empty
    | otherwise = (end . foldr kind empty . Map.toList) e
    
writeClassInfo :: [Class] -> Doc
writeClassInfo [] = empty
writeClassInfo xs = (scat . (text "Class Constraints:" :) . map pclass) xs

writeInstInfo :: [Inst] -> Doc
writeInstInfo [] = empty
writeInstInfo xs = (scat . (text "Instance Constraints:" :) . map pinst) xs    
  

kind :: (KcEntry, Kind) -> Doc -> Doc 
kind (e,k) ac = pinfo e k $+$ ac 

writeAssumps :: [Assumption] -> Doc
writeAssumps [] = empty
writeAssumps xs = (scat . (text "Types Infered:" :) . map passump) xs
    
passump :: Assumption -> Doc
passump (i :>: t) 
    = hsep [pprint i, dcolon, text (prettyPrint t)] 
    
pinst :: Inst -> Doc
pinst i 
    = passt (instsupers i) <+> n' <+> ts'
      where
        n' = text (show (instname i))
        ts' = hsep (map (text . prettyPrint) (instparameters i)) 
        
pclass :: Class -> Doc
pclass c 
    = passt (supers c) <+> n' <+> ts'
      where
        n' = text (show (name c))
        ts' = hsep (map (text . prettyPrint) (parameters c))         

passt :: Context -> Doc
passt [] = empty
passt xs 
    = parens (passt' xs) <+> text "=>"
      where
        passt' [y] = pprint' y
        passt' (y:ys) = pprint' y <+> text "," <+> passt' ys
        pprint' x = text (prettyPrint x)  

-- some auxiliar functions

scat = foldr ($+$) empty

end x = text "Kinds Infered:" $+$ x

pinfo e k = hsep [pprint (identry e), dcolon, pprint' k]
            where
               pprint' = text . prettyPrint

pprint x = text (show x)
dcolon = text "::"             

isClassKind e = idty e == IdClass

isTypeKind e = idty e == IdType       
