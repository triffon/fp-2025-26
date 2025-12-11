module Lists where

import Prelude hiding (head, tail, null, length, enumFromTo,
                      (++), reverse, (!!), elem, init, last, take, drop,
                      map, filter, foldr, foldl, foldr1, foldl1,
                      scanl, scanr, zip, unzip, zipWith, takeWhile)

head :: [a] -> a
head (h:_) = h

tail :: [a] -> [a]
tail (_:t) = t
-- >>> head [1,2,3]
-- 1

-- >>> tail [1,2,3]
-- [2,3]

null :: [a] -> Bool
null [] = True
null _  = False

length :: [a] -> Int
{-
length []     = 0
length (_:xs) = 1 + length xs
-}

length = foldr (const (+1)) 0

-- >>> length [1,2,3]
-- 3

-- >>> [1..5]
-- [1,2,3,4,5]

-- !!!! enumFromTo a a = [a]
enumFromTo a b
  | a == b    = [a]
  | otherwise = a : enumFromTo (succ a) b

-- >>> enumFromTo 1 10
-- [1,2,3,4,5,6,7,8,9,10]

-- >>> enumFromTo 'a' 'z'
-- "abcdefghijklmnopqrstuvwxyz"

-- >>> enumFromTo False True
-- [False,True]

-- >>> succ (1, 2)
-- No instance for `Enum (Integer, Integer)'
--   arising from a use of `it_aQPo'
-- In the first argument of `evalPrint', namely `it_aQPo'
-- In a stmt of an interactive GHCi command: evalPrint it_aQPo

(++) :: [a] -> [a] -> [a]
{-
[]     ++ l = l
(x:xs) ++ l = x : xs ++ l
-}

l1 ++ l2 = foldr (:) l2 l1 

-- >>> [1..3] ++ [5..7]
-- [1,2,3,5,6,7]

reverse :: [a] -> [a]
{-
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]
-}

-- reverse = foldr (\x -> (++[x])) []

rcons :: [a] -> a -> [a]
rcons xs x = x : xs
reverse = foldl rcons []

-- >>> reverse [[1..5],[7..11],[11..15]]
-- [[11,12,13,14,15],[7,8,9,10,11],[1,2,3,4,5]]

-- >>> reverse [1..10]
-- [10,9,8,7,6,5,4,3,2,1]

{-
(!!) :: (Eq t1, Num t1) => [t2] -> t1 -> t2
[] !! _ = error "Не можем да индексираме празен списък!"
(x:xs) !! n
  | n == 0     = x
  | otherwise  = xs !! (n - 1)
-}

(!!) :: (Eq t1, Num t1) => [t2] -> t1 -> t2
[] !! _ = error "Не можем да индексираме празен списък!"
(x:_)  !! 0 = x
(_:xs) !! n = xs !! (n - 1)

-- >>> [1..5] !! 3
-- 4

-- >>> [1..5] !! 10
-- Не можем да индексираме празен списък!

-- >>> [] !! 5
-- Не можем да индексираме празен списък!

elem :: Eq t => t -> [t] -> Bool
{-
elem _ [] = False
elem x (y:ys) = x == y || elem x ys
-}

elem x = foldr (\y -> (x == y||)) False

-- !!!! elem x (x:_) = True
{-
elem x (y:ys)
  | x == y = True
  | otherwise = elem x ys
-}


{-
  elem x l = not (null l) && case l of (y:ys) -> x == y || elem x ys
-}

-- >>> elem 3 [1..5]
-- True

-- >>> elem 6 [1..5]
-- False

-- >>> take 5 (show [1..5])
-- "[1,2,"

-- >>> (read "(2,4.5)")::(Int, Double)
-- (2,4.5)

-- >>> :t 2
-- 2 :: Num a => a
-- >>> :t 3.5
-- 3.5 :: Fractional a => a

-- >>> :t (+)
-- (+) :: Num a => a -> a -> a

-- >>> :t (/)
-- (/) :: Fractional a => a -> a -> a

-- >>> :t div
-- div :: Integral a => a -> a -> a

-- >>> 2 / 3
-- 0.6666666666666666

-- >>> (2 :: Int) / (3 :: Int)
-- No instance for `Fractional Int' arising from a use of `/'
-- In the expression: (2 :: Int) / (3 :: Int)
-- In an equation for `it_aVFd': it_aVFd = (2 :: Int) / (3 :: Int)

-- >>> fromIntegral (2 :: Int) / fromIntegral(3 :: Int)
-- 0.6666666666666666

pythagoreanTriples a b = [ (x, y, z) | x <- [a..b], y <- [x+1..b], z <- [y+1..b],
                                       x^2 + y^2 == z^2, gcd x y == 1 ]
-- >>> pythagoreanTriples 1 200
-- [(3,4,5),(5,12,13),(7,24,25),(8,15,17),(9,40,41),(11,60,61),(12,35,37),(13,84,85),(15,112,113),(16,63,65),(17,144,145),(19,180,181),(20,21,29),(20,99,101),(24,143,145),(28,45,53),(28,195,197),(33,56,65),(36,77,85),(39,80,89),(44,117,125),(48,55,73),(51,140,149),(52,165,173),(57,176,185),(60,91,109),(65,72,97),(85,132,157),(88,105,137),(95,168,193),(104,153,185),(119,120,169)]

init :: [a] -> [a]
init [_]    = []
init (x:xs) = x:init xs


-- >>> init [1..5]
-- [1,2,3,4,5]

last :: [t] -> t
{-
last [x]    = x
last (_:xs) = last xs
-}
-- last = foldr1 (\x r -> r)
last = foldr1 (const id)

-- >>> last [1..5]
-- 5

take :: (Eq t, Num t) => t -> [a] -> [a]
take 0 _      = []
take _ []     = error "Не можем да извличаме елементи от празен списък!"
take n (x:xs) = x:take (n-1) xs

-- >>> take 4 [1..10]
-- [1,2,3,4]

-- >>> take 20 [1..10]
-- Не можем да извличаме елементи от празен списък!

drop :: (Eq t, Num t) => t -> [a] -> [a]
drop 0 l      = l
drop _ []     = []
drop n (_:xs) = drop (n-1) xs

-- >>> drop 4 [1..10]
-- [5,6,7,8,9,10]

-- >>> drop 20 [1..10]
-- []

map :: (t -> a) -> [t] -> [a]
{-
map _ []     = []
map f (x:xs) = f x:map f xs
-}

map f = foldr (\x -> (f x:)) []

-- >>> map (+1) [1..5]
-- [2,3,4,5,6]

filter :: (a -> Bool) -> [a] -> [a]

{-
filter _ [] = []
filter p (x:xs)
 | p x       = x:rest
 | otherwise = rest
   where rest = filter p xs
filter p (x:xs) = if p x then x:rest else rest
   where rest = filter p xs
-}

filter p = foldr (\x -> if p x then (x:) else id) []

-- >>> filter odd [1..12]
-- [1,3,5,7,9,11]

-- >>> [ (x, y) | x <- [1..3], y <- [5..7]]
-- [(1,5),(1,6),(1,7),(2,5),(2,6),(2,7),(3,5),(3,6),(3,7)]

-- >>> concat (map (\x -> map (\y -> (x, y)) [5..7]) [1..3])
-- [(1,5),(1,6),(1,7),(2,5),(2,6),(2,7),(3,5),(3,6),(3,7)]

foldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr _  nv []     = nv
foldr op nv (x:xs) = x `op` foldr op nv xs

-- >>> foldr (+) 0 [1..5]
-- 15

foldl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldl _ nv [] = nv
foldl op nv (x:xs) = foldl op (nv `op` x) xs

-- >>> foldl (+) 0 [1..5]
-- 15

-- >>> :t (:)
-- (:) :: a -> [a] -> [a]
-- >>> foldl (:) [] [1..5]
-- Couldn't match type `a_a5qgp[sk:1]' with `[a_a5qgp[sk:1]]'
-- Expected: [a_a5qgp[sk:1]] -> [[a_a5qgp[sk:1]]] -> [a_a5qgp[sk:1]]
--   Actual: [a_a5qgp[sk:1]] -> [[a_a5qgp[sk:1]]] -> [[a_a5qgp[sk:1]]]
-- `a_a5qgp[sk:1]' is a rigid type variable bound by
--   the inferred type of it_a5qeC :: [a_a5qgp[sk:1]]
--   at /home/trifon/fmisync/Courses/2025_26/FP_2025_26/fp-2025-26/lectures/haskell/Lists.hs:251:2-20
-- In the first argument of `foldl', namely `(:)'
-- In the expression: foldl (:) [] [1 .. 5]
-- In an equation for `it_a5qeC': it_a5qeC = foldl (:) [] [1 .. 5]
-- Relevant bindings include
--   it_a5qeC :: [a_a5qgp[sk:1]]
--     (bound at /home/trifon/fmisync/Courses/2025_26/FP_2025_26/fp-2025-26/lectures/haskell/Lists.hs:251:2)

foldr1 :: (t -> t -> t) -> [t] -> t
foldr1 op [x]    = x
foldr1 op (x:xs) =  x `op` foldr1 op xs

-- >>> foldr1 (+) [1..5]
-- 15

foldl1 :: (t2 -> t2 -> t2) -> [t2] -> t2
foldl1 op (x:xs) = foldl op x xs

scanr :: (t -> a -> a) -> a -> [t] -> [a]
{-
scanr op nv [] = [nv]
scanr op nv (x:xs) = x `op` hr:rest
  where rest@(hr:_) = scanr op nv xs
-}
scanr op nv = foldr (\x rest@(hr:_) -> x `op` hr:rest) [nv]

-- >>> scanr (+) 0 [1..6]
-- [21,20,18,15,11,6,0]

scanl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> [t1]
scanl op nv [] = [nv]
scanl op nv (x:xs) = nv:scanl op (nv `op` x) xs

-- >>> scanl (+) 0 [1..6]
-- [0,1,3,6,10,15,21]

-- >>> zip [1..10] [20..24]
-- [(1,20),(2,21),(3,22),(4,23),(5,24)]

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

-- >>> unzip (zip [1..10] [20..24])
-- ([1,2,3,4,5],[20,21,22,23,24])
 
unzip :: [(a1, a2)] -> ([a1], [a2])
unzip = foldr (\(x,y) (xs,ys)-> (x:xs,y:ys)) ([], [])

-- >>> unzip [(1,20),(2,21),(3,22),(4,23),(5,24)]
-- ([1,2,3,4,5],[20,21,22,23,24])

zipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith _  [] _  = []
zipWith _  _  [] = []
zipWith op (x:xs) (y:ys) = op x y : zipWith op xs ys

-- >>> takeWhile (<3) [1,2,3,0,8]
-- [1,2]

-- >>> dropWhile (<3) [1,2,3,0,8]
-- [3,0,8]

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (\x r -> if p x then x : r else []) []
