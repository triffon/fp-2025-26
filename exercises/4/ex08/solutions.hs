-- Решения на задачите от Упражнение 8: Кортежи, Списъци, List Comprehension, Type Classes

-- ============================================
-- Секция 1: Класове от типове
-- ============================================

-- Задача 1.1: clamp
clamp :: (Ord a) => a -> a -> a -> a
clamp x minVal maxVal
  | x < minVal = minVal
  | x > maxVal = maxVal
  | otherwise = x

-- ============================================
-- Секция 2: Кортежи
-- ============================================

-- Задача 2.1: swap
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Задача 2.2: distance
distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- Задача 2.3: midpoint
midpoint :: (Double, Double) -> (Double, Double) -> (Double, Double)
midpoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

-- Задача 2.4: triangleArea
triangleArea :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
triangleArea p1 p2 p3 = abs ((x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) / 2)
  where
    (x1, y1) = p1
    (x2, y2) = p2
    (x3, y3) = p3

-- ============================================
-- Секция 3: Списъци
-- ============================================

-- Задача 3.1: mySum
mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

-- Задача 3.2: myReverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Задача 3.3: myConcat
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x : xs) = x ++ myConcat xs

-- Задача 3.4: myDrop
myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop n xs@(_ : rest)
  | n <= 0 = xs
  | otherwise = myDrop (n - 1) rest

-- ============================================
-- Секция 4: List Comprehension
-- ============================================

-- Задача 4.1: cubes - кубове на числата от 1 до 10
cubes :: [Int]
cubes = [x ^ 3 | x <- [1 .. 10]]

-- Задача 4.2: divisibleBy3And5 - числа от 1 до 100, които се делят на 3 и 5
divisibleBy3And5 :: [Int]
divisibleBy3And5 = [x | x <- [1 .. 100], x `mod` 3 == 0, x `mod` 5 == 0]

-- Бонус: с генератор за списъци
divisibleBy3And5Bonus :: [Int]
divisibleBy3And5Bonus = [15, 30 .. 100]

-- Задача 4.3: pythagoreanTriples
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = [(a, b, c) | a <- [1 .. n], b <- [1 .. n], c <- [1 .. n], a ^ 2 + b ^ 2 == c ^ 2]

-- Задача 4.4: factors
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- Задача 4.5: primesUpTo
primesUpTo :: Int -> [Int]
primesUpTo n = [x | x <- [2 .. n], isPrime x]
  where
    isPrime k = length (factors k) == 2

-- Задача 4.6: removeDuplicates
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates [y | y <- xs, y /= x]

-- ============================================
-- Секция 5: Комбинирани задачи
-- ============================================

-- Задача 5.1: listStats
listStats :: [Int] -> (Int, Double, Int, Int)
listStats xs = (total, avg, minVal, maxVal)
  where
    total = mySum xs
    avg = fromIntegral (mySum xs) / fromIntegral (length xs)
    minVal = minimum xs
    maxVal = maximum xs

-- Задача 5.2: perfectNumbers
perfectNumbers :: Int -> [Int]
perfectNumbers n = [x | x <- [1 .. n], isPerfect x]
  where
    isPerfect k = sum (init (factors k)) == k

-- Задача 5.3: zipWithIndex
zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex xs = zip [0 ..] xs

-- Задача 5.4: groupByValue
groupByValue :: (Eq a, Ord a) => [a] -> [(a, Int)]
groupByValue [] = []
groupByValue xs = sortByCount [(x, count x xs) | x <- removeDuplicates xs]
  where
    count val list = length [y | y <- list, y == val]
    sortByCount pairs = reverse (sortPairs pairs)
    sortPairs [] = []
    sortPairs (p : ps) =
      sortPairs [x | x <- ps, snd x < snd p]
        ++ [p]
        ++ sortPairs [x | x <- ps, snd x >= snd p]

-- Задача 5.5: decode (с поддръжка за многоцифрени числа - бонус)

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

decode :: String -> String
decode [] = []
decode (c : cs)
  | isDigit c =
      let (numStr, rest) = takeDigits (c : cs)
          num = read numStr :: Int
          (letter : remaining) = rest
       in replicate num letter ++ decode remaining
  | otherwise = c : decode cs
  where
    -- Помощна функция за извличане на всички последователни цифри
    takeDigits :: String -> (String, String)
    takeDigits s = (digits, rest)
      where
        digits = takeWhile isDigit s
        rest = dropWhile isDigit s

-- ============================================
-- Тестови извиквания
-- ============================================
main :: IO ()
main = do
  putStrLn "=== Тестови извиквания ===\n"

  -- Секция 1: Класове от типове
  putStrLn "=== Секция 1: Класове от типове ==="
  putStrLn "Задача 1.1: clamp"
  putStrLn $ "clamp 5 1 10 = " ++ show (clamp 5 1 10)
  putStrLn $ "clamp 15 1 10 = " ++ show (clamp 15 1 10)
  putStrLn $ "clamp (-5) 1 10 = " ++ show (clamp (-5) 1 10)
  putStrLn ""

  -- Секция 2: Кортежи
  putStrLn "=== Секция 2: Кортежи ==="
  putStrLn "Задача 2.1: swap"
  putStrLn $ "swap (1, 2) = " ++ show (swap (1, 2))
  putStrLn $ "swap (\"hello\", 42) = " ++ show (swap ("hello", 42))
  putStrLn ""

  putStrLn "Задача 2.2: distance"
  putStrLn $ "distance (0, 0) (3, 4) = " ++ show (distance (0, 0) (3, 4))
  putStrLn $ "distance (1, 1) (4, 5) = " ++ show (distance (1, 1) (4, 5))
  putStrLn ""

  putStrLn "Задача 2.3: midpoint"
  putStrLn $ "midpoint (0, 0) (4, 6) = " ++ show (midpoint (0, 0) (4, 6))
  putStrLn $ "midpoint (1, 2) (5, 8) = " ++ show (midpoint (1, 2) (5, 8))
  putStrLn ""

  putStrLn "Задача 2.4: triangleArea"
  putStrLn $ "triangleArea (0, 0) (4, 0) (0, 3) = " ++ show (triangleArea (0, 0) (4, 0) (0, 3))
  putStrLn ""

  -- Секция 3: Списъци
  putStrLn "=== Секция 3: Списъци ==="
  putStrLn "Задача 3.1: mySum"
  putStrLn $ "mySum [1, 2, 3, 4] = " ++ show (mySum [1, 2, 3, 4])
  putStrLn $ "mySum [] = " ++ show (mySum ([] :: [Int]))
  putStrLn ""

  putStrLn "Задача 3.2: myReverse"
  putStrLn $ "myReverse [1, 2, 3, 4] = " ++ show (myReverse [1, 2, 3, 4])
  putStrLn $ "myReverse \"hello\" = " ++ show (myReverse "hello")
  putStrLn ""

  putStrLn "Задача 3.3: myConcat"
  putStrLn $ "myConcat [[1, 2], [3, 4], [5]] = " ++ show (myConcat [[1, 2], [3, 4], [5]])
  putStrLn $ "myConcat [\"hello\", \" \", \"world\"] = " ++ show (myConcat ["hello", " ", "world"])
  putStrLn ""

  putStrLn "Задача 3.4: myDrop"
  putStrLn $ "myDrop 2 [1, 2, 3, 4, 5] = " ++ show (myDrop 2 [1, 2, 3, 4, 5])
  putStrLn $ "myDrop 0 [1, 2, 3] = " ++ show (myDrop 0 [1, 2, 3])
  putStrLn $ "myDrop 5 [1, 2] = " ++ show (myDrop 5 [1, 2])
  putStrLn ""

  -- Секция 4: List Comprehension
  putStrLn "=== Секция 4: List Comprehension ==="
  putStrLn "Задача 4.1: cubes"
  putStrLn $ "cubes = " ++ show cubes
  putStrLn ""

  putStrLn "Задача 4.2: divisibleBy3And5"
  putStrLn $ "divisibleBy3And5 = " ++ show divisibleBy3And5
  putStrLn $ "divisibleBy3And5Bonus = " ++ show divisibleBy3And5Bonus
  putStrLn ""

  putStrLn "Задача 4.3: pythagoreanTriples"
  putStrLn $ "pythagoreanTriples 10 = " ++ show (pythagoreanTriples 10)
  putStrLn ""

  putStrLn "Задача 4.4: factors"
  putStrLn $ "factors 12 = " ++ show (factors 12)
  putStrLn $ "factors 7 = " ++ show (factors 7)
  putStrLn ""

  putStrLn "Задача 4.5: primesUpTo"
  putStrLn $ "primesUpTo 20 = " ++ show (primesUpTo 20)
  putStrLn ""

  putStrLn "Задача 4.6: removeDuplicates"
  putStrLn $ "removeDuplicates [1, 2, 2, 3, 1, 4] = " ++ show (removeDuplicates [1, 2, 2, 3, 1, 4])
  putStrLn $ "removeDuplicates \"hello\" = " ++ show (removeDuplicates "hello")
  putStrLn ""

  -- Секция 5: Комбинирани задачи
  putStrLn "=== Секция 5: Комбинирани задачи ==="
  putStrLn "Задача 5.1: listStats"
  putStrLn $ "listStats [1, 5, 3, 9, 2] = " ++ show (listStats [1, 5, 3, 9, 2])
  putStrLn $ "listStats [10] = " ++ show (listStats [10])
  putStrLn ""

  putStrLn "Задача 5.2: perfectNumbers"
  putStrLn $ "perfectNumbers 30 = " ++ show (perfectNumbers 30)
  putStrLn ""

  putStrLn "Задача 5.3: zipWithIndex"
  putStrLn $ "zipWithIndex ['a', 'b', 'c'] = " ++ show (zipWithIndex ['a', 'b', 'c'])
  putStrLn $ "zipWithIndex [10, 20, 30] = " ++ show (zipWithIndex [10, 20, 30])
  putStrLn ""

  putStrLn "Задача 5.4: groupByValue"
  putStrLn $ "groupByValue [1, 2, 2, 3, 3, 3, 1] = " ++ show (groupByValue [1, 2, 2, 3, 3, 3, 1])
  putStrLn $ "groupByValue \"hello\" = " ++ show (groupByValue "hello")
  putStrLn ""

  putStrLn "Задача 5.5: decode"
  putStrLn $ "decode \"a2bd5c\" = " ++ show (decode "a2bd5c")
  putStrLn $ "decode \"a2b12d5c\" = " ++ show (decode "a2b12d5c")
  putStrLn ""

  putStrLn "=== Край на тестовете ==="
