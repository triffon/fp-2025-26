import Prelude hiding (repeat)

repeat :: a -> [a]
repeat x = x : repeat x

hailstone :: Int -> [Int]
hailstone 1 = repeat 1
hailstone n
  | even n, let p = n `div` 2 = p : hailstone p
  | otherwise, let p = 3 * n + 1 = p : hailstone p

hailstone' :: Int -> [Int]
hailstone' = iterate next
  where
    next :: Int -> Int
    next 1 = 1
    next n
      | even n = n `div` 2
      | otherwise = 3 * n + 1

rationals :: [(Int, Int)]
rationals = [(x - y, y) | x <- [0..], y <- [1..x]]

pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = [(a, b, c) | c <- [1..], a <- [1..c], b <- [1..c], a ^ 2 + b ^ 2 == c ^ 2, a < b]

sieve :: [Int]
sieve = sieveHelper [2..]
  where
    sieveHelper :: [Int] -> [Int]
    sieveHelper (x:xs) = x : sieveHelper (filter (\y -> y `rem` x /= 0) xs)

generateExponents :: Int -> Int -> [Int]
generateExponents k l = [z | z <- [1..], x <- [1..z], y <- [1..z], x ^ k * y ^ l == z]

outsideHyp :: Int -> Int -> Int -> [(Int, Int)]
outsideHyp a b c = [(u + a, v + b) | z <- [(c+1)..], u <- [-z..z], v <- [-z..z], z == u * v]

forestFire :: [Int]
forestFire = map a [0..]
  where
    a :: Int -> Int
    a 0 = 1
    a n =
      let arithmetic = [2 * a (n - k) - a (n - 2 * k) | k <- [1..(n `div` 2)]]
      in head $ dropWhile (`elem` arithmetic) [1..]