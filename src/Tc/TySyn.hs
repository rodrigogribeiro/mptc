
module Tc.TySyn where


-- this module defines a data type
-- that represent type synonyms.
-- This type is used to expand type
-- synonyms before type inference,
-- and to write / read interface files

import Language.Haskell.Exts


type TySyn = (Type, Type) -- first component: left hand side of type synonym
                          -- second component: right hand side.

tySynName :: TySyn -> QName
tySynName (t,_) 
    = goLeft t
      where
         goLeft (TyApp l _) = goLeft l
         goLeft (TyCon qn) = qn                           