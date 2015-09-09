{-#LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction#-}
module Tests.Data.Full.Teste3 where

import Tests.Data.Full.Teste2


teste = [h]

f = (1,'a', [1])

g x = x : [2]

h1 = ('1',())

teste3 = teste1 '1'

teste1 x = teste3