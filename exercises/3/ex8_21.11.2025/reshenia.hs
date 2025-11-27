

repeat1 :: ( a -> a ) -> Integer -> (a -> a)
repeat1 _ 0 = id
-- repeat1 f n = \ x ->  repeat1 f (n - 1) (f x)
repeat1 f n = repeat1 f (n - 1) . f

($$) :: (Float, Float) -> (Float, Float) -> Float
(x1, y1) $$ (x2, y2) =  sqrt ((x2 - x1)^2 + (y2 - y1)^2)

len1 :: Num a1 => [a2] -> a1
len1 [] = 0
len1 (x:xs) = 1 + len1 xs

foldr2 :: (a -> Int -> Int) -> Int -> [a] -> Int
foldr2 f n [] = n
foldr2 f n (x:xs) = foldr2 f (f x n) xs

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x : map f xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 p (x:xs) = if p x then x : filter2 p xs else filter2 p xs

len2 :: [a] -> Int
len2 = foldr2 (\ x res -> 1 + res) 0


isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
-- isInfixOf (x:xs) (y: ys) = x == y && isInfixOf xs ys || isInfixOf (x:xs) ys 
isInfixOf l (y:ys) = isPrefixOf l (y:ys) || isInfixOf l ys
    where isPrefixOf [] _ = True
          isPrefixOf _ [] = False
          isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

join :: [[Char]] -> Char -> [Char]
join [] _ = ""
join (x:xs) ch = x ++ [ch] ++ join xs ch


insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x <= y then x: (y:ys) else y: insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []


quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =  quickSort (filter (<= x) xs) ++ [x] ++ quickSort (filter (> x) xs)

quickSort2 :: (a -> a -> Bool) -> [a] -> [a]
quickSort2 cmp [] = []
quickSort2 cmp (x:xs) =  quickSort2 cmp (filter (cmp x) xs) ++ [x] ++ quickSort2 cmp (filter (not . cmp x) xs)

longestSublist :: ([a] -> Bool) -> [a] -> [a]
longestSublist p [] = []
longestSublist p (x:xs) = let lhs = longestPrefix p (x:xs)
                              rhs = longestSublist p xs 
            in if length lhs >= length rhs then lhs else rhs 


longestPrefix p xs = helper p xs [] []
    where helper p [] prefix maxPrefix = maxPrefix
          helper p (y : ys) prefix maxPrefix = let newPrefix = prefix ++ [y]
            in if p newPrefix then helper p ys newPrefix newPrefix else helper p ys newPrefix maxPrefix

scalarProduct :: Num a => (a, a, a) -> (a, a, a) -> a
scalarProduct (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1* y2 + z1 * z2

vectorProduct :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
vectorProduct (a1, a2, a3) (b1, b2, b3) = (a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1)
