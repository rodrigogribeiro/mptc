
module Iface.IfaceReader where

 
-- this module reads a interface file
-- and load all type information stored
-- in it. It uses a parsec based parser.

import Control.Monad.Identity
import Control.Applicative hiding ((<|>), many)
import Data.Char
import Data.List
import qualified Data.Map as Map

import Language.Haskell.Exts hiding (name,parse, AssocRight)
import System.Posix.Directory (getWorkingDirectory)
import System.FilePath.Posix (pathSeparator)
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

import Iface.Iface

import Tc.Assumption
import Tc.Class
import Tc.Kc.KcEnv
import Tc.TySyn
import Tc.TcLabel

import Utils.ErrMsg (interfaceFileParserError)
import Utils.Id

-- a type for interface data

parseInterface :: FilePath -> IO Iface
parseInterface hifile
    = do
        c <- readFile hifile        
        either (interfaceFileParserError . show) return (pinterface c)
        
pinterface :: String -> Either ParseError Iface
pinterface = parse piface "ifacefile"         

-- starting the definition of the parsec parser

type Parser a = ParsecT String () Identity a

piface :: Parser Iface
piface 
    = Iface <$> pifacename <*> psyns <*> plabels <*> pkenv <*> 
                pclasses <*> pinsts  <*> passumps

pifacename :: Parser Id
pifacename 
    = f <$> sepBy identifier dot <* semi
      where
         f xs = toId $ Ident (intercalate "." xs)    

psyns :: Parser [TySyn]
psyns = reserved "synonyms" *> braces syns
        where
           syns = endBy psyn semi
           psyn = (\t1 _ t2 -> (t1,t2)) <$> typ <*> symbol "=" <*> typ    
           
plabels :: Parser LabelEnv
plabels = reserved "labels" *> braces labelsets
          where
             labelsets = Map.fromList <$> many labelset
             labelset = (,) <$> idp <*> braces labels
             labels = endBy plbl semi
             plbl = (\i _ t -> (i,t)) <$> idp <*> dcolon <*> typ               

pkenv :: Parser KcEnv        
pkenv = reserved "kinds" *> braces (f <$> kc <*> kt) <?> "Kind Env"
        where
            f c = foldr (uncurry insertTypeKind) (h c)
            h = foldr (uncurry insertClassKind) emptyEnv
            kc = reserved "kindofclasses" *> braces nkparserl
            kt = reserved "kindoftypes" *> braces nkparserl
            nkparserl = try (endBy nkp semi)
            nkp = (\i _ k -> (i,k)) <$> idp <*> dcolon 
                                            <*> kindp

kindp :: Parser Kind
kindp 
    = chainr1 kp ka
      where
        kp = KindParen <$> parens kindp <|> star
        star = const KindStar <$> symbol "*"
        ka = const KindFn <$> symbol "->"
        
idp :: Parser Id
idp = toId <$> (identp <|> parens symp)
      where
        identp = (UnQual . Ident) <$> identifier
        symp = (UnQual . Symbol) <$> operator
                               

pclasses :: Parser [Class]
pclasses = (\_ c -> c) <$> reserved "classes" <*> 
                  braces (try (endBy pclass semi))
                  <?> "Classes"

pclass :: Parser Class
pclass = f <$> psupers <*> idp <*> many1 typ 
         where
            f s i ts = Class i ts s [] []
            
psupers :: Parser Context
psupers 
    = try (parens (commaSep passt) <* symbol "=>") <|> return []

passt :: Parser Asst
passt = f <$> idp <*> many1 typ
        where
          f i = ClassA (unid i)
          
       

pinsts :: Parser [Inst]
pinsts = (\_ c -> c) <$> reserved "instances" <*> 
                     braces (try (endBy pinst semi))
                     <?> "Instances"

pinst :: Parser Inst
pinst = f <$> psupers <*> idp <*> many1 typ
        where
           f s i ts = Inst i ts s


passumps :: Parser [Assumption]
passumps = (\_ c -> c) <$> reserved "assumptions" <*> 
                     braces (try (endBy passump semi))
                     <?> "Assumptions"

passump :: Parser Assumption
passump = (\i _ t -> i :>: t) <$> idp <*> dcolon <*> pscheme 

pscheme :: Parser Type
pscheme = TyForall Nothing <$> option [] psupers <*> typ      

-- the type parser

typ :: Parser Type
typ = buildExpressionParser [[Infix parr AssocRight]] btype
      where
        parr = const TyFun <$> symbol "->"        
        btype = foldl1 TyApp <$> many1 (atype <* whiteSpace)
        atype = try gtycon              <|> 
                try tuplety             <|> 
                TyParen <$> parens typ  <|>
                listty                  <|>
                tyvar
        gtycon = try unitcon  <|>
                 try arrowcon <|>
                 try tuplecon <|>
                 listcon      <|>
                 qtycon
        tyvar = (TyVar . Ident) <$> varp
        tuplety = try (parens t2p)
        listty = TyList <$> try (brackets typ)
        t2p = (\t _ t' ts -> TyTuple Boxed (t:t':ts)) <$>
                typ <*> comma <*> typ <*> commaSep' typ
        unitcon = const (TyCon (Special UnitCon)) <$>
                    symbol "()"
        listcon = const (TyCon (Special ListCon)) <$>
                    symbol "[]"
        arrowcon = const (TyCon (Special FunCon)) <$>
                    parens (symbol "->")     
        tuplecon = TyCon . Special . TupleCon Boxed . length <$> parens (many1 comma)  
        qtycon = (TyCon . UnQual . Ident) <$> conp  
        commaSep' p = option [] (comma *> commaSep p)                                                                   
        
commaSep p = p `sepBy` comma        

-- lexer definition

lexer = P.makeTokenParser haskellDef

reserved = P.reserved lexer
braces = P.braces lexer
semi = P.semi lexer
lexeme = P.lexeme lexer
identifier = P.identifier lexer
dcolon = P.symbol lexer "::" 
symbol = P.symbol lexer
parens = P.parens lexer
operator = P.operator lexer
brackets = P.brackets lexer
comma = symbol ","
dot = symbol "."
whiteSpace = P.whiteSpace lexer

varp = list <$> satisfy isLower <*> option [] identifier         
         
conp = list <$> satisfy isUpper <*> option [] identifier

list c cs = c:cs         

        
