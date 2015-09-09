{-#LANGUAGE DeriveDataTypeable#-}

module Utils.Id ( Id,
                  ToId(..),
                  Unqualify(..),
                  isqual,
                  isunqual,
                  unid 
                 ) where

import Data.Generics 
import Language.Haskell.Exts

import Utils.ErrMsg(unsupportedDeclMsg)

import Utils.Debug

-- representation of Ids, that represent
-- internal names for the compiler

data Id = IdName Name
        | IdQName QName
        | IdSpecial SpecialCon
        deriving (Data, Typeable)

-- a type class to convert such thing to Id's

class ToId n where
   -- convert to id
   toId :: n -> Id

instance ToId Name where
   toId = IdName
   
instance ToId QName where
   toId = IdQName

instance ToId SpecialCon where
   toId = IdSpecial

instance ToId QOp where
   toId (QVarOp qn) = toId qn
   toId (QConOp qn) = toId qn

instance ToId Op where
   toId (VarOp n) = toId n
   toId (ConOp n) = toId n

instance ToId Id where
   toId = id
   
instance ToId TyVarBind where
   toId (UnkindedVar n) = toId n
   toId x = unsupportedDeclMsg x   

unid (IdName n) = UnQual n
unid (IdQName n) = n
unid (IdSpecial n) = Special n
   
-- a class for unqualify names

class Unqualify n where
   unqual :: n -> Name

instance Unqualify Name where
   unqual = id

instance Unqualify Op where
   unqual (VarOp n) = n
   unqual (ConOp n) = n

instance Unqualify QOp where
   unqual (QVarOp n) = unqual n
   unqual (QConOp n) = unqual n

instance Unqualify QName where
   unqual (Qual _ n) = n
   unqual (UnQual n) = n
   unqual (Special con) = unqual con

instance Unqualify SpecialCon where
   unqual UnitCon       = Symbol "()"
   unqual ListCon       = Symbol "[]"
   unqual FunCon        = Symbol "->"
   unqual Cons          = Symbol ":"
   unqual (TupleCon _ n) = Symbol (concat ["(", replicate (n - 1) ',', ")"])
                

-- custom instances

instance Eq Id where
   (IdName x)    == (IdName y)    = x == y
   (IdQName x)   == (IdQName y)   = (unqual x) == (unqual y)
   (IdSpecial x) == (IdSpecial y) = (unqual x) == (unqual y)
   x             == y              
                    | isqual x = (unqual' x) == y
                    | isqual y = x == (unqual' y)
                    | otherwise = (show x) == (show y) 
                    
instance Ord Id where
   (IdName x)    <= (IdName y)    = x <= y
   (IdQName x)   <= (IdQName y)   = x <= y
   (IdSpecial x) <= (IdSpecial y) = x <= y
   x             <= y
                | isqual x = (unqual' x) <= y
                | isqual y = x <= (unqual' y)
                | otherwise = (show x) <= (show y)
instance Show Id where
   show (IdName x)    = prettyPrint x
   show (IdQName x)   = prettyPrint x
   show (IdSpecial x) = prettyPrint x

-- some test functions

isqual :: Id -> Bool
isqual (IdName _) = False
isqual _          = True

isunqual = not . isqual

unqual' (IdQName x) = IdName (unqual x)
unqual' (IdSpecial x) = IdName (unqual x)
