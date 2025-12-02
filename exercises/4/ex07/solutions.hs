-- Решения на задачите от Упражнение 7: Основни понятия в Haskell

-- ============================================
-- Задача 2.1: average
-- ============================================
average :: Double -> Double -> Double -> Double
average x y z = (x + y + z) / 3

-- ============================================
-- Задача 2.2: isTriangle
-- ============================================
isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c = (a + b > c) && (a + c > b) && (b + c > a)

-- ============================================
-- Задача 3.1: subtractFrom100
-- ============================================
subtractFrom100 :: Int -> Int
subtractFrom100 = (100 -)

-- ============================================
-- Задача 3.2: subtract3
-- ============================================
subtract3 :: Int -> Int
subtract3 x = x - 3

-- Алтернативно решение:
-- subtract3 = subtract 3

-- ============================================
-- Задача 3.3: isGreaterThan50
-- ============================================
isGreaterThan50 :: Int -> Bool
isGreaterThan50 = (> 50)

-- ============================================
-- Задача 4.1: circleArea (с let)
-- ============================================
circleArea :: Double -> Double
circleArea r =
  let pi = 3.141592
   in pi * r * r

-- ============================================
-- Задача 4.2: cylinderVolume (с where)
-- ============================================
cylinderVolume :: Double -> Double -> Double
cylinderVolume r h = baseArea * h
  where
    baseArea = circleArea r

-- ============================================
-- Задача 4.3: vectorAngleCos
-- ============================================
vectorAngleCos :: Double -> Double -> Double -> Double -> Double
vectorAngleCos a1 a2 b1 b2 =
  let lengthA = vecLength a1 a2
      lengthB = vecLength b1 b2
      dotProduct = a1 * b1 + a2 * b2
   in dotProduct / (lengthA * lengthB)
  where
    vecLength x y = sqrt (x * x + y * y)

-- ============================================
-- Задача 5.1: gradeLetter (с guards)
-- ============================================
gradeLetter :: Int -> String
gradeLetter grade
  | grade == 6 = "Отличен"
  | grade == 5 = "Много добър"
  | grade == 4 = "Добър"
  | grade == 3 = "Среден"
  | grade == 2 = "Слаб"
  | otherwise = "Невалидна оценка"

-- ============================================
-- Задача 5.2: triangleType (с guards)
-- ============================================
triangleType :: Double -> Double -> Double -> String
triangleType a b c
  | not (isTriangle a b c) = "Невалиден тригълник"
  | a == b && b == c = "Равностранен"
  | a == b || a == c || b == c = "Равнобедрен"
  | otherwise = "Разностранен"

-- ============================================
-- Задача 6.1: safeDivide (с pattern matching)
-- ============================================
safeDivide :: Double -> Double -> Double
safeDivide _ 0 = 0.0
safeDivide x y = x / y

-- ============================================
-- Задача 7.1: quadrant (с guards)
-- ============================================
quadrant :: Double -> Double -> Int
quadrant x y
  | x > 0 && y > 0 = 1
  | x < 0 && y > 0 = 2
  | x < 0 && y < 0 = 3
  | x > 0 && y < 0 = 4
  | otherwise = 0 -- точка на осите

-- ============================================
-- Задача 7.2: isPalindromeNumber
-- ============================================
isPalindromeNumber :: Int -> Bool
isPalindromeNumber n = show n == reverse (show n)

-- Алтернативно решение с математически операции:
-- isPalindromeNumber n = n == reverseNumber n 0
--   where reverseNumber 0 acc = acc
--         reverseNumber num acc = reverseNumber (num `div` 10) (acc * 10 + num `mod` 10)

-- ============================================
-- Задача 7.3: triangleNum
-- ============================================
triangleNum :: Int -> Int
triangleNum n = n * (n + 1) `div` 2

-- Алтернативно решение с рекурсия:
-- triangleNum 0 = 0
-- triangleNum n = n + triangleNum (n - 1)

-- ============================================
-- Задача 7.4: binomeCoef
-- ============================================
binomeCoef :: Int -> Int -> Int
binomeCoef n 0 = 1
binomeCoef n k
  | n < k = 0
  | otherwise = binomeCoef (n - 1) k + binomeCoef (n - 1) (k - 1)

-- ============================================
-- Тестови извиквания
-- ============================================
main :: IO ()
main = do
  putStrLn "=== Тестови извиквания ===\n"

  -- Задача 2.1
  putStrLn "Задача 2.1: average"
  putStrLn $ "average 10 20 30 = " ++ show (average 10 20 30)
  putStrLn ""

  -- Задача 2.2
  putStrLn "Задача 2.2: isTriangle"
  putStrLn $ "isTriangle 3 4 5 = " ++ show (isTriangle 3 4 5)
  putStrLn $ "isTriangle 1 2 5 = " ++ show (isTriangle 1 2 5)
  putStrLn ""

  -- Задача 3.1
  putStrLn "Задача 3.1: subtractFrom100"
  putStrLn $ "subtractFrom100 37 = " ++ show (subtractFrom100 37)
  putStrLn ""

  -- Задача 3.2
  putStrLn "Задача 3.2: subtract3"
  putStrLn $ "subtract3 37 = " ++ show (subtract3 37)
  putStrLn ""

  -- Задача 3.3
  putStrLn "Задача 3.3: isGreaterThan50"
  putStrLn $ "isGreaterThan50 75 = " ++ show (isGreaterThan50 75)
  putStrLn $ "isGreaterThan50 50 = " ++ show (isGreaterThan50 50)
  putStrLn $ "isGreaterThan50 25 = " ++ show (isGreaterThan50 25)
  putStrLn ""

  -- Задача 4.1
  putStrLn "Задача 4.1: circleArea"
  putStrLn $ "circleArea 5.0 = " ++ show (circleArea 5.0)
  putStrLn ""

  -- Задача 4.2
  putStrLn "Задача 4.2: cylinderVolume"
  putStrLn $ "cylinderVolume 3.0 5.0 = " ++ show (cylinderVolume 3.0 5.0)
  putStrLn ""

  -- Задача 4.3
  putStrLn "Задача 4.3: vectorAngleCos"
  putStrLn $ "vectorAngleCos 1 0 2 0 = " ++ show (vectorAngleCos 1 0 2 0)
  putStrLn $ "vectorAngleCos 1 1 1 0 = " ++ show (vectorAngleCos 1 1 1 0)
  putStrLn $ "vectorAngleCos 1 0 0 1 = " ++ show (vectorAngleCos 1 0 0 1)
  putStrLn $ "vectorAngleCos 1 1 (-1) 0 = " ++ show (vectorAngleCos 1 1 (-1) 0)
  putStrLn $ "vectorAngleCos 1 0 (-1) 0 = " ++ show (vectorAngleCos 1 0 (-1) 0)
  putStrLn ""

  -- Задача 5.1
  putStrLn "Задача 5.1: gradeLetter"
  putStrLn $ "gradeLetter 5 = " ++ gradeLetter 5
  putStrLn $ "gradeLetter 6 = " ++ gradeLetter 6
  putStrLn $ "gradeLetter 2 = " ++ gradeLetter 2
  putStrLn ""

  -- Задача 5.2
  putStrLn "Задача 5.2: triangleType"
  putStrLn $ "triangleType 3 3 3 = " ++ triangleType 3 3 3
  putStrLn $ "triangleType 3 3 4 = " ++ triangleType 3 3 4
  putStrLn $ "triangleType 3 4 5 = " ++ triangleType 3 4 5
  putStrLn $ "triangleType 3 4 10 = " ++ triangleType 3 4 10
  putStrLn ""

  -- Задача 6.1
  putStrLn "Задача 6.1: safeDivide"
  putStrLn $ "safeDivide 10 2 = " ++ show (safeDivide 10 2)
  putStrLn $ "safeDivide 10 0 = " ++ show (safeDivide 10 0)
  putStrLn ""

  -- Задача 7.1
  putStrLn "Задача 7.1: quadrant"
  putStrLn $ "quadrant 3 4 = " ++ show (quadrant 3 4)
  putStrLn $ "quadrant (-2) 5 = " ++ show (quadrant (-2) 5)
  putStrLn $ "quadrant (-1) (-3) = " ++ show (quadrant (-1) (-3))
  putStrLn $ "quadrant 4 (-2) = " ++ show (quadrant 4 (-2))
  putStrLn ""

  -- Задача 7.2
  putStrLn "Задача 7.2: isPalindromeNumber"
  putStrLn $ "isPalindromeNumber 121 = " ++ show (isPalindromeNumber 121)
  putStrLn $ "isPalindromeNumber 123 = " ++ show (isPalindromeNumber 123)
  putStrLn $ "isPalindromeNumber 1221 = " ++ show (isPalindromeNumber 1221)
  putStrLn ""

  -- Задача 7.3
  putStrLn "Задача 7.3: triangleNum"
  putStrLn $ "triangleNum 1 = " ++ show (triangleNum 1)
  putStrLn $ "triangleNum 3 = " ++ show (triangleNum 3)
  putStrLn $ "triangleNum 5 = " ++ show (triangleNum 5)
  putStrLn ""

  -- Задача 7.4
  putStrLn "Задача 7.4: binomeCoef"
  putStrLn $ "binomeCoef 4 0 = " ++ show (binomeCoef 4 0)
  putStrLn $ "binomeCoef 4 1 = " ++ show (binomeCoef 4 1)
  putStrLn $ "binomeCoef 4 2 = " ++ show (binomeCoef 4 2)
  putStrLn $ "binomeCoef 4 3 = " ++ show (binomeCoef 4 3)
  putStrLn $ "binomeCoef 4 4 = " ++ show (binomeCoef 4 4)
  putStrLn $ "binomeCoef 10 3 = " ++ show (binomeCoef 10 3)
  putStrLn $ "binomeCoef 15 5 = " ++ show (binomeCoef 15 5)
  putStrLn ""

  putStrLn "=== Край на тестовете ==="
