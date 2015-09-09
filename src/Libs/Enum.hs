
module Enum where

import Base

instance Enum () where
    toEnum 0           = ()
    fromEnum ()        = 0
    enumFrom ()        = [()]
