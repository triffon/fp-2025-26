import Prelude hiding (filter, zipWith, dropWhile)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter pred (x:xs)
  | pred x = x : filter pred xs
  | otherwise = filter pred xs

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile pred l@(x:xs)
  | pred x = dropWhile pred xs
  | otherwise = l

rotate :: Int -> [a] -> [a]
rotate n l = let r = n `rem` length l
  in drop r l ++ take r l

removeEvery :: Int -> [a] -> [a]
removeEvery _ [] = []
removeEvery n l = take (n - 1) l ++ removeEvery n (drop n l)

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf l r@(y:ys) = l `isPrefixOf` r || l `isInfixOf` ys
  where
    isPrefixOf :: Eq a => [a] -> [a] -> Bool
    [] `isPrefixOf` _ = True
    _ `isPrefixOf` [] = False
    (x:xs) `isPrefixOf` (y:ys) = x == y && xs `isPrefixOf` ys
    
pairSum :: Int -> [Int] -> [(Int, Int)]
pairSum n l = [(x, y) | x <- l, y <- l, x + y == n, x <= y]

quickSortBy :: (a -> a -> Bool) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy cmp (x:xs) =
  let less = filter (`cmp` x) xs
      greater = filter (\y -> not (y `cmp` x)) xs
  in quickSortBy cmp less ++ [x] ++ quickSortBy cmp greater

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = let s = subsets xs
  in map (x:) s ++ s

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations l = [x:xs | x <- l, xs <- permutations (filter (/= x) l)]

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (dropWhile (== x) xs)

maximumBy :: (a -> a -> Bool) -> [a] -> a
maximumBy cmp = foldl1 (\ result current -> if result `cmp` current then current else result)