{-#LANGUAGE NoImplicitPrelude#-}
module Eq where

-- some boilerplate Eq definitions

import Base

instance Eq () where
    () == ()  =  True
    
-- tuples

instance (Eq a, Eq b) => Eq (a,b) where
    (a,b) == (a',b') = a == a' && b == b'

instance (Eq a, Eq b, Eq c) => Eq (a,b,c) where
    (a,b,c) == (a',b',c') = a == a' && b == b' && c == c'

instance (Eq a, Eq b, Eq c, Eq d) => Eq (a,b,c,d) where
    (a,b,c,d) == (a',b',c',d') = a == a' && b == b' 
                                         && c == c'
                                         && d == d'
                                         
instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a,b,c,d,e) where
    (a,b,c,d,e) == (a',b',c',d',e') = a == a' && 
                                      b == b' && 
                                      c == c' && 
                                      d == d' &&
                                      e == e'
                                      
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq (a,b,c,d,e,f) where
    (a,b,c,d,e,f) == (a',b',c',d',e',f') = a == a' && 
                                           b == b' && 
                                           c == c' && 
                                           d == d' &&
                                           e == e' &&
                                           f == f'
                                                                                  
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (a,b,c,d,e,f,g) where
    (a,b,c,d,e,f,g) == (a',b',c',d',e',f',g') 
                = a == a' && 
                  b == b' && 
                  c == c' && 
                  d == d' &&
                  e == e' &&    
                  f == f' &&
                  g == g'