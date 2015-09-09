
module Bounded where

import Base

instance Bounded () where
    minBound = ()
    maxBound = ()

instance (Bounded a, Bounded b) => Bounded (a,b) where
    minBound = (minBound, minBound)
    maxBound = (maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c) => Bounded (a,b,c) where
    minBound = (minBound, minBound, minBound)
    maxBound = (maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d) => Bounded (a,b,c,d) where
    minBound = (minBound, minBound, minBound,minBound)
    maxBound = (maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d,Bounded e) => Bounded (a,b,c,d,e) where
    minBound = (minBound, minBound, minBound,minBound, minBound)
    maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound)
    
instance (Bounded a, Bounded b, Bounded c, Bounded d,Bounded e, Bounded f) => Bounded (a,b,c,d,e,f) where
    minBound = (minBound, minBound, minBound,minBound, minBound, minBound)
    maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)
    
instance (Bounded a, Bounded b, Bounded c, Bounded d,Bounded e, Bounded f, Bounded g) => Bounded (a,b,c,d,e,f,g) where
    minBound = (minBound, minBound, minBound,minBound, minBound, minBound, minBound)
    maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound, maxBound, maxBound)
    