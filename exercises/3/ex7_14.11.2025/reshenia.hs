fib3 :: Integer -> Integer
fib3 0 = 0
fib3 1 = 1
fib3 n = fib3 (n-1) + fib3 (n-2)


fastPow :: Integer -> Integer -> Integer
fastPow _ 0 = 1
fastPow x n 
    | even n = sqHalf 
    | otherwise = x * sqHalf
    where 
        half = fastPow x (div n 2)
        sqHalf = half * half

isSquare :: (Eq t, Num t) => t -> t -> Bool
isSquare 1 1 = True
isSquare a b 
    | a == b = False
    | a * a == b = True
    | otherwise = isSquare (a + 1) b
    

sumSquares :: Integer -> Integer -> Integer
sumSquares a b 
    | a == b = isSquareA
    | otherwise =  sumSquares (a + 1) b + isSquareA
    where isSquareA = if isSquare 1 a then a else 0

repeat1 :: (a -> a) -> Int -> a -> a
repeat1 _ 0 = id
repeat1 f n = \x -> f (repeat1 f (n - 1) x)

