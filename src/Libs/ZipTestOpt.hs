module ZipTestOpt where


import PreludeIO

zip :: [a] -> [b] -> [(a,b)]
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

zip :: [a] -> [b] -> [c] -> [(a,b,c)]
zip []     _      _      = []
zip _      []     _      = []
zip _      _      []     = []
zip (x:xs) (y:ys) (z:zs) = (x,y,z) : zip xs ys zs

   
test = do print ( zip [1] [2] )
          print ( zip [1] [2] [3])

