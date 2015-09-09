
module Iface.IfaceWriter (writeInterface) where


-- this module does the recording of a
-- interface file. Interface files, 
-- contains information about kinds,
-- types and classes.

import Data.List
import qualified Data.Map as Map

import Language.Haskell.Exts hiding (name)
import System.Posix.Directory (getWorkingDirectory)
import System.FilePath.Posix (pathSeparator)

import Tc.Assumption
import Tc.Class
import Tc.Kc.KcEnv
import Tc.TySyn
import Tc.TcLabel

import Text.PrettyPrint.HughesPJ

import Utils.Id
import Utils.FileNameUtils


-- this function writes the interface file 

writeInterface :: FilePath -> ModuleName -> Id -> [TySyn] -> LabelEnv ->
                  KcEnv -> [Class] -> [Inst] -> [Assumption] -> IO ()
writeInterface dir (ModuleName s) i syns lbls kc cls is as
    = do
        i' <- writeId i
        sy <- writeSynInfo syns
        k <- writeKindInfo kc
        l <- writeLabelInfo lbls
        c <- writeConstraintInfo cls is
        a <- writeAssumptionInfo as
        let s' = reverse $ takeWhile (/= '.') $ reverse s
        writeFile (interfaceFileName dir s') (show (scat [i',sy,l,k,c,a]))
        
writeId :: Id -> IO Doc
writeId i = return (text (show i) <+> semi)        
        
-- this function generates the Doc that
-- store all type synonym information in
-- the interface file

writeSynInfo :: [TySyn] -> IO Doc
writeSynInfo 
    = return . end . braces . scat . map writeSyn
      where
         end x = text "synonyms" <> x
         
writeLabelInfo :: LabelEnv -> IO Doc
writeLabelInfo
    = return . end . braces . scat . map writeLabels . Map.toList
      where
         end x = text "labels" <> x
         
writeLabels :: (Id, [Label]) -> Doc
writeLabels (i,ls) = text (show i) <+> braces (scat (map writeLabel ls))         

writeLabel :: Label -> Doc
writeLabel (i,t) = text (show i) <+> dcolon <+> text (prettyPrint t) <> semi                 
         
writeSyn :: TySyn -> Doc
writeSyn (l,r) 
    = pprint' l <+> text "=" <+> pprint' r <> semi
      where
        pprint' = text . prettyPrint           
                        
    
-- this function generates the Doc that
-- store all kind information in a interface
-- file    
    
writeKindInfo :: KcEnv -> IO Doc
writeKindInfo 
    = return . end . foldr kind s . Map.toList
      where
        -- first element holds class info, second type info
        s = ([], [])
        

kind :: (KcEntry, Kind) -> ([Doc], [Doc]) -> ([Doc], [Doc])
kind (e,k) (ck,tk) 
    | isClassKind e = (pinfo e k : ck, tk)  
    | isTypeKind e = (ck, pinfo e k : tk)
    | otherwise = (ck,tk)
    
-- this function generates the doc that store
-- all information about constraints defined
-- in current module

writeConstraintInfo :: [Class] -> [Inst] -> IO Doc
writeConstraintInfo cs is 
    = return (classInfo cs $+$ instInfo is)
        
classInfo :: [Class] -> Doc
classInfo cs 
    = text "classes" <> braces (scat (map pclass cs))
    
pclass :: Class -> Doc
pclass c 
    = passt (supers c) <+> n' <+> ts' <> semi
      where
        n' = text (show (name c))
        ts' = hsep (map (text . prettyPrint) (parameters c))    

instInfo :: [Inst] -> Doc
instInfo is
    = text "instances" <> braces (scat (map pinst is))
    
pinst :: Inst -> Doc
pinst i 
    = passt (instsupers i) <+> n' <+> ts' <> semi
      where
        n' = text (show (instname i))
        ts' = hsep (map (text . prettyPrint) (instparameters i)) 

passt :: Context -> Doc
passt [] = empty
passt xs 
    = parens (passt' xs) <+> text "=>"
      where
        passt' [y] = pprint' y
        passt' (y:ys) = pprint' y <+> text "," <+> passt' ys
        pprint' x = text (prettyPrint x) 
  


-- this function generates the doc that store
-- all information about all assumptions of
-- type signatures, data constructors, record
-- projections.

writeAssumptionInfo :: [Assumption] -> IO Doc
writeAssumptionInfo as 
    = return $ text "assumptions" <> 
                 braces (scat (map passump as))
    
passump :: Assumption -> Doc
passump (i :>: t) 
    = hsep [pprint i, dcolon, text (prettyPrint t), semi]                
    
-- some auxiliar functions

scat = foldr ($+$) empty

end (c,t) 
    = pack c' t'
      where
        c' = scat c --(intersperse semi c)
        t' = scat t --(intersperse semi t)
        
pack c t 
    = hcat [text "kinds", info]
      where
         info = braces (cls $+$ tys)
         cls = nest 3 (hcat[text "kindofclasses", braces c])
         tys = nest 3 (hcat[text "kindoftypes", braces t])                         

pprint x = text (show x)
dcolon = text "::"

pinfo e k = hsep [pprint (identry e), dcolon, pprint' k, semi]
            where
               pprint' = text . prettyPrint
               
isClassKind e = idty e == IdClass

isTypeKind e = idty e == IdType
