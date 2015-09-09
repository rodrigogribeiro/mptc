
module Tc.Assumption (Assumption (..), toScheme) where

import Language.Haskell.Exts

import Utils.Id

-- a module for representing a data type for
-- type assumptions

data Assumption = Id :>: Type 
                  deriving (Eq, Ord)
                  
instance Show Assumption where
   show (i :>: ty) = show i ++ " :: " ++ prettyPrint ty        
    
toScheme n ctx = (toId n :>:) . TyForall Nothing ctx
