
module BuiltIn.BuiltInTypes where


import Language.Haskell.Exts

-- this module defines some static symbol for represent
-- BuiltIn types for Haskell

-- int type

tInt :: Type
tInt = TyCon (UnQual (Ident "Int"))

-- char type

tChar :: Type
tChar = TyCon (UnQual (Ident "Char"))

-- bool type

tBool :: Type
tBool = TyCon (UnQual (Ident "Bool"))

-- double type

tDouble :: Type
tDouble = TyCon (UnQual (Ident "Double"))

-- float type

tFloat :: Type
tFloat = TyCon (UnQual (Ident "Float"))

-- integer type

tInteger :: Type
tInteger = TyCon (UnQual (Ident "Integer"))

-- string type

tString :: Type
tString = tList tChar

tString1 :: Type
tString1 = TyCon (UnQual (Ident "String"))


-- list type

tList :: Type -> Type
tList  = TyList 

-- num constraint

numconstr :: Type -> Asst
numconstr v = ClassA (UnQual (Ident "Num")) [v]

-- fractional constraint

fracconstr :: Type -> Asst
fracconstr v = ClassA (UnQual (Ident "Fractional")) [v]

-- ord constraint

ordconstr :: Type -> Asst
ordconstr v = ClassA (UnQual (Ident "Num")) [v]

-- integral constraint

integralconstr :: Type -> Asst
integralconstr v = ClassA (UnQual (Ident "Integral")) [v]

-- enum constraint

enumconstr :: Type -> Asst
enumconstr v = ClassA (UnQual (Ident "Enum")) [v]

-- monad constraint

monadconstr :: Type -> Asst
monadconstr v = ClassA (UnQual (Ident "Monad")) [v]
