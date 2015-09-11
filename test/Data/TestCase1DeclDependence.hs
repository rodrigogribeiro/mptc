
module Data.TestCase1DeclDependence where

a = 1

b = a

c = h b d

d = c

f = g h a

g = undefined f

h = undefined g
