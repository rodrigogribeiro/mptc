
module PreludeList (
   map, (++), filter, concat, concatMap, head, last, tail, init, null,
   length, (!!), foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr,
   scanr1, iterate, repeat, replicate, cycle, take, drop, splitAt, 
   takeWhile, dropWhile, span, break, lines, words, unlines, unwords,
   reverse, and, or, any, all, elem, notElem, lookup, sum, product,
   maximum, mininum, zip, zip3, zipWith, zipWith3, unzip, unzip3 ) 
where

import PreludeBase

infixl 9 !!
infixr 5 ++
infix 4 `elem`, `notElem`

isSpace :: Char -> Bool
isSpace x = x `elem` " \t\r\n"

-- map and append

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) | p x = x : filter p xs
                | otherwise = filter p xs
                
concat :: [[a]] -> [a]
concat = foldr (++) []

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

head :: [a] -> a
head (x:_) = x
head _     = error "Prelude.head: empty list"

tail :: [a] -> [a]
tail (_:xs) = xs
tail _      = error "Prelude.tail: empty list"

last :: [a] -> a
last [x] = x
last (_:xs) = last xs
last []  = error "Prelude.last: empty list"

init :: [a] -> [a]
init [x] = []
init (x:xs) = x : init xs
init [] = error "Prelude.init: empty list"

null :: [a] -> Bool
null [] = True
null _  = False 

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

(!!) :: [a] -> Int -> a
xs !! n | n < 0 = error "Prelude.!!: negative index"
[] !! _         = error "Prelude.!!: empty list"
(x:_) !! 0      = x
(_:xs) !! n     = xs !! (n - 1)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs
foldl1 f [] = error "Prelude.foldl1: empty list"

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs = q : (case xs of
                        [] -> []
                        x : xs -> scanl f (f q x) xs)
                        
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs) = scanl f x xs
scanl1 _ [] = []

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)
foldr1 _ [] = error "Prelude.foldr1: empty list"

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 [] = [q0]
scanr f q0 (x:xs) = f x q : qs
                    where
                        qs@(q:_) = scanr f q0 xs
                        
scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 f [] = []
scanr1 f [x] = [x]
scanr1 f (x:xs) = f x q : qs
                where 
                    qs@(q:_) = scanr1 f xs
                    
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate n = take n . repeat

cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle xs = xs ++ cycle xs

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_:xs) = drop (n - 1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs)
            | p x = x : takeWhile p xs
            | otherwise = []
            
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs)
            | p x = dropWhile p xs
            | otherwise = xs
            
span, break :: (a -> Bool) -> [a] -> ([a], [a])
span p [] = ([],[])
span p xs@(x:xs')
            | p x = (x:ys,zs)
            | otherwise = ([],xs)
            where (ys,zs) = span p xs'
                          
break p = span (not . p)


lines :: String -> [String]
lines [] = []
lines s = let (l, s') = break (== '\n') s
            in l : case s' of
                        [] -> []
                        (_:s'') -> lines s''
                        
words :: String -> [String]
words s = case dropWhile isSpace s of
            "" -> []
            s' -> w : words s''
                  where 
                     (w, s'') = break isSpace s'
                     
unlines :: [String] -> String 
unlines = concatMap (++ "\n")

unwords :: [String] -> String 
unwords [] = []
unwords ws = foldr1 (\w s -> w ++ (' ':s)) ws

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

and, or :: [Bool] -> Bool
and = foldr (&&) True 

or = foldr (||) False 

any, all :: (a -> Bool) -> [a] -> Bool
any p = or . map p
all p = and . map p

elem, notElem :: Eq a => a -> [a] -> Bool
elem x = any (== x)
notElem x = all (/= x)

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup key [] = Nothing 
lookup key ((x,y):xs)
        | x == key = Just y
        | otherwise = lookup key xs
        
sum, product :: Num a => [a] -> a
sum = foldr (+) 0
product = foldr (*) 1

maximum, minimum :: Ord a => [a] -> a
maximum [] = error "Prelude.maximum: empty list"
maximum xs = foldr1 max xs

minimum [] = error "Prelude.minimum: empty list"
minimum xs = foldr1 min xs

zip :: [a] -> [b] -> [(a,b)]
zip = zipWith (\a b -> (a,b))

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 = zipWith3 (\a b c -> (a,b,c))

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _ _ = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f (a:as) (b:bs) (c:cs) = f a b c : zipWith3 f as bs cs
zipWith3 _ _ _ _ = []


unzip :: [(a,b)] -> ([a],[b])
unzip = foldr (\(a,b) ~(as,bs) -> (a:as, b:bs)) ([],[])

unzip3 :: [(a,b,c)] -> ([a],[b],[c])
unzip3 = foldr (\(a,b,c) ~(as,bs,cs) -> (a:as, b:bs, c:cs)) ([],[],[])


instance Eq a => Eq [a]  where 
    [] == [] = True
    (x:xs) == (y:ys) = (x == y) && (xs == ys)
    _ == _ = False 

sequence :: Monad m => [m a] -> m [a]
sequence ms = foldr k (return []) ms
            where
              k m m' = do { x <- m; xs <- m'; return (x:xs) }

sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ . map f



