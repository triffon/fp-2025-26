module Lists where

import Prelude hiding (head, tail, null, length, enumFromTo,
                      (++), reverse, (!!), elem, init, last, take, drop)

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
length []     = 0
length (_:xs) = 1 + length xs

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
[]     ++ l = l
(x:xs) ++ l = x : xs ++ l

-- >>> [1..3] ++ [5..7]
-- [1,2,3,5,6,7]

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- >>> reverse [[1..5],[7..1x0],[11..15]]
-- [[11,12,13,14,15],[7,8,9,10],[1,2,3,4,5]]

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
elem _ [] = False
elem x (y:ys) = x == y || elem x ys

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
-- [1,2,3,4]

last :: [t] -> t
last [x]    = x
last (_:xs) = last xs

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

