-- Решения на задачите от Упражнение 10: Лениво оценяване. Потоци. Композиция и апликация.

-- ============================================
-- Секция 2: Потоци (Streams)
-- ============================================

-- Задача 2.1: squares - безкраен списък от квадратите на естествените числа
squares :: [Integer]
squares = map (^ 2) [0 ..] -- Или squares = [x ^ 2 | x <- [0 ..]]

-- Задача 2.2: factorials - безкраен списък от факториели
factorials :: [Integer]
factorials = 1 : zipWith (*) factorials [1 ..]

-- Задача 2.3: primes - безкраен списък от прости числа (сито на Ератостен)
sieve :: [Integer] -> [Integer]
sieve (p : xs) = p : sieve (filter (\x -> x `mod` p /= 0) xs) -- Или: sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes :: [Integer]
primes = sieve [2 ..]

-- Задача 2.4: mergeStreams - обединява два сортирани безкрайни списъка без дубликати (уникално сливане)
mergeStreams :: (Ord a) => [a] -> [a] -> [a]
mergeStreams [] ys = ys
mergeStreams xs [] = xs
mergeStreams (x : xs) (y : ys)
  | x < y = x : mergeStreams xs (y : ys)
  | x > y = y : mergeStreams (x : xs) ys
  | otherwise = x : mergeStreams xs ys -- Премахваме дубликатите

-- ============================================
-- Секция 3: Композиция на функции
-- ============================================

-- Задача 3.1: productOfPositiveSquares - произведение на квадратите на положителните числа
productOfPositiveSquares :: [Integer] -> Integer
productOfPositiveSquares = product . map (^ 2) . filter (> 0)

-- Задача 3.2: sumOfCubesLength - сума на дължините (броя цифри) на кубовете
-- Помощна функция за броя на цифрите
numDigits :: Integer -> Int
numDigits n
  | n < 10 = 1
  | otherwise = 1 + numDigits (n `div` 10)

sumOfCubesLength :: [Integer] -> Int
sumOfCubesLength = sum . map (numDigits . (^ 3))

-- ============================================
-- Секция 4: Оператори за апликация
-- ============================================

-- Задача 4.1: Препишете израз с $
-- Преди: length (filter (>10) (map (*2) [1..20]))
-- След:
task41 :: Int
task41 = length $ filter (> 10) $ map (* 2) [1 .. 20]

-- ============================================
-- Секция 5: Комбинирани задачи
-- ============================================

-- Задача 5.1: streamZipWith - прилага функция върху съответните елементи на два безкрайни списъка
streamZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
streamZipWith f (x : xs) (y : ys) = f x y : streamZipWith f xs ys
streamZipWith _ _ _ = []

-- Задача 5.2: hamming - безкраен списък от числа на Хаминг
-- Използваме mergeStreams като оператор, за да премахнем нуждата от скоби, възползвайки се от приоритета на операторите. Това е най-хаскелския подход
hamming :: [Integer]
hamming = 1 : map (* 2) hamming `mergeStreams` map (* 3) hamming `mergeStreams` map (* 5) hamming

-- Горното може да се запише и по един от следните 2 начина:
-- 1. Най-класическата имплементация, която бихме ползвали в други езици.
-- hamming = 1 : mergeStreams (map (* 2) hamming) (mergeStreams (map (* 3) hamming) $ map (* 5) hamming)
-- 2. Чрез апликация (но все още ни трябват някои скоби). Важно е да използваме : като ф-ция, а не като оператор! Това променя приоритета й.
-- hamming = (:) 1 $ mergeStreams (map (* 2) hamming) $ mergeStreams (map (* 3) hamming) $ map (* 5) hamming

-- Задача 5.3: streamInterleave - преплита два безкрайни списъка
streamInterleave :: [a] -> [a] -> [a]
streamInterleave (x : xs) ys = x : streamInterleave ys xs
streamInterleave [] ys = ys

-- Задача 5.4: streamProcess - комплексна обработка на поток
streamProcess :: (Num a) => (a -> a) -> a -> (a -> Bool) -> Int -> (a -> a) -> a
streamProcess f x p n g = sum $ map g $ take n $ filter p $ iterate f x

-- Задача 5.5: collatz - поредица на Колатц
collatz :: Integer -> [Integer]
collatz n = n : collatz (nextCollatz n)
  where
    nextCollatz x
      | even x = x `div` 2
      | otherwise = 3 * x + 1

-- ============================================
-- Тестови извиквания
-- ============================================
main :: IO ()
main = do
  putStrLn "=== Тестови извиквания ===\n"

  -- Секция 2: Потоци
  putStrLn "=== Секция 2: Потоци ==="
  putStrLn "Задача 2.1: squares"
  putStrLn $ "take 5 squares = " ++ show (take 5 squares)
  putStrLn ""

  putStrLn "Задача 2.2: factorials"
  putStrLn $ "take 6 factorials = " ++ show (take 6 factorials)
  putStrLn ""

  putStrLn "Задача 2.3: primes"
  putStrLn $ "take 10 primes = " ++ show (take 10 primes)
  putStrLn ""

  putStrLn "Задача 2.4: mergeStreams"
  putStrLn $ "take 10 (mergeStreams [2, 4..] [3, 6..]) = " ++ show (take 10 (mergeStreams [2, 4 ..] [3, 6 ..]))
  putStrLn ""

  -- Секция 3: Композиция
  putStrLn "=== Секция 3: Композиция на функции ==="
  putStrLn "Задача 3.1: productOfPositiveSquares"
  putStrLn $ "productOfPositiveSquares [-2, 3, -1, 4] = " ++ show (productOfPositiveSquares [-2, 3, -1, 4])
  putStrLn ""

  putStrLn "Задача 3.2: sumOfCubesLength"
  putStrLn $ "sumOfCubesLength [1, 2, 3, 4] = " ++ show (sumOfCubesLength [1, 2, 3, 4])
  putStrLn ""

  -- Секция 4: Оператори за апликация
  putStrLn "=== Секция 4: Оператори за апликация ==="
  putStrLn "Задача 4.1: task41"
  putStrLn $ "task41 = " ++ show task41
  putStrLn $ "Оригиналният израз: length (filter (>10) (map (*2) [1..20])) = " ++ show (length (filter (> 10) (map (* 2) [1 .. 20])))
  putStrLn ""

  -- Секция 5: Комбинирани задачи
  putStrLn "=== Секция 5: Комбинирани задачи ==="
  putStrLn "Задача 5.1: streamZipWith"
  putStrLn $ "take 5 (streamZipWith (+) [1..] [10, 20..]) = " ++ show (take 5 (streamZipWith (+) [1 ..] [10, 20 ..]))
  putStrLn $ "take 5 (streamZipWith (*) [2..] [3, 5..]) = " ++ show (take 5 (streamZipWith (*) [2 ..] [3, 5 ..]))
  putStrLn ""

  putStrLn "Задача 5.2: hamming"
  putStrLn $ "take 20 hamming = " ++ show (take 20 hamming)
  putStrLn ""

  putStrLn "Задача 5.3: streamInterleave"
  putStrLn $ "take 10 (streamInterleave [1, 3..] [2, 4..]) = " ++ show (take 10 (streamInterleave [1, 3 ..] [2, 4 ..]))
  putStrLn ""

  putStrLn "Задача 5.4: streamProcess"
  putStrLn $ "streamProcess (*2) 1 even 5 (^2) = " ++ show (streamProcess (* 2) 1 even 5 (^ 2))
  putStrLn ""

  putStrLn "Задача 5.5: collatz"
  putStrLn $ "take 10 (collatz 13) = " ++ show (take 10 (collatz 13))
  putStrLn $ "take 15 (collatz 13) = " ++ show (take 15 (collatz 13))
  putStrLn ""

  putStrLn "=== Край на тестовете ==="
