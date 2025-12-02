module Lists where

import Prelude hiding (head, tail, null, length, enumFromTo,
                      (++), reverse, (!!), elem)

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
-- !!!! elem x (x:_) = True
{-
elem x (y:ys)
  | x == y = True
  | otherwise = elem x ys
-}
elem x (y:ys) = x == y || elem x ys

{-
  elem x l = not (null l) && case l of (y:ys) -> x == y || elem x ys
-}

-- >>> elem 3 [1..5]
-- True

-- >>> elem 6 [1..5]
-- False

