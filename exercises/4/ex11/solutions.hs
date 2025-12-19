-- Решения на задачите от Упражнение 11: Подготовка за контролно

import Data.List (nub, transpose)

-- ============================================
-- Задача 1: Групиране по свойство
-- ============================================

groupBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
groupBy _ [] = []
groupBy keyFunc xs = [(key, elementsWithKey key) | key <- keys]
  where
    keys = nub (map keyFunc xs) -- Уникални ключове в реда на първото срещане
    elementsWithKey k = filter (\x -> keyFunc x == k) xs

-- ============================================
-- Задача 2: Прозорци със условие
-- ============================================

conditionalWindows :: (a -> Bool) -> Int -> [a] -> [[a]]
conditionalWindows _ _ [] = []
conditionalWindows p n all@(x : xs)
  | p x && length all >= n = take n all : conditionalWindows p n xs
  | otherwise = conditionalWindows p n xs

-- Алтернативно решение с генератор:
-- conditionalWindows' :: (a -> Bool) -> Int -> [a] -> [[a]]
-- conditionalWindows' p n xs = [window | (i, x) <- zip [0 ..] xs, p x, i + n <= length xs, let window = take n (drop i xs)]

-- ============================================
-- Задача 3: Вложени трансформации
-- ============================================

nestedMap :: [(a -> a)] -> [a] -> [[a]]
nestedMap fs xs = [scanl (flip ($)) x fs | x <- xs]

-- Обяснение: scanl прилага функциите последователно и връща всички междинни резултати
-- flip ($) обръща реда на аргументите, за да може да подаваме функцията към стойността

-- Алтернативно решение без scanl:
-- nestedMap' :: [(a -> a)] -> [a] -> [[a]]
-- nestedMap' fs xs = map (applyAll fs) xs
--   where
--     applyAll funcs x = x : foldl (\acc f -> acc ++ [f (last acc)]) [] funcs

-- ============================================
-- Задача 4: Потоци с памет
-- ============================================

streamWithMemory :: (Int -> Int -> Int) -> Int -> Int -> [Int]
streamWithMemory f start memory = start : streamWithMemory f (f start memory) start

-- Обяснение: първият елемент е start, следващият е f start memory,
-- а за следващия memory става start (запомняме предишния елемент)

-- ============================================
-- Задача 5: Сливане на сортирани списъци
-- ============================================

-- Помощна функция за сливане на два сортирани списъка
merge2 :: (Ord a) => [a] -> [a] -> [a]
merge2 [] ys = ys
merge2 xs [] = xs
merge2 (x : xs) (y : ys)
  | x <= y = x : merge2 xs (y : ys)
  | otherwise = y : merge2 (x : xs) ys

-- Главна функция - слива списък от сортирани списъци
mergeLists :: (Ord a) => [[a]] -> [a]
mergeLists [] = []
mergeLists [xs] = xs
mergeLists (x : xs) = merge2 x (mergeLists xs)

-- Алтернативно решение с foldl:
-- mergeLists' :: (Ord a) => [[a]] -> [a]
-- mergeLists' = foldl merge2 []

-- ============================================
-- Задача 6: Матрична транспозиция с условие
-- ============================================

selectiveTranspose :: (a -> Bool) -> [[a]] -> [[a]]
selectiveTranspose p matrix
  | satisfyingCount >= totalCount `div` 2 = transpose matrix
  | otherwise = matrix
  where
    allElements = concat matrix
    totalCount = length allElements
    satisfyingCount = length (filter p allElements)

-- ============================================
-- Задача 7: Сума в списък от списъци
-- ============================================

deepSum :: (Ord a, Num a) => (a -> Bool) -> [[a]] -> a
deepSum p xss = sum [x | xs <- xss, x <- xs, p x]

-- Алтернативно решение с функции от по-висок ред:
-- deepSum p = sum . map sum . map (filter p)
-- Или още по-кратко:
-- deepSum p = sum . filter p . concat

-- ============================================
-- Задача 8: Матрично завъртане
-- ============================================

-- Функция за завъртане на матрица 90 градуса по часовниковата стрелка
rotateClockwise :: [[a]] -> [[a]]
rotateClockwise = map reverse . transpose

-- Функция за завъртане на матрица 90 градуса обратно на часовниковата стрелка
rotateCounterClockwise :: [[a]] -> [[a]]
rotateCounterClockwise = reverse . transpose

-- Главна функция за завъртане n пъти
rotateMatrix :: Int -> [[a]] -> [[a]]
rotateMatrix n matrix
  | n == 0 = matrix
  | n > 0 = rotateMatrix (n - 1) (rotateClockwise matrix)
  | otherwise = rotateMatrix (n + 1) (rotateCounterClockwise matrix)

-- Алтернативно решение с fold:
-- rotateMatrix' :: Int -> [[a]] -> [[a]]
-- rotateMatrix' n matrix
--   | n >= 0 = iterate rotateClockwise matrix !! (n `mod` 4)
--   | otherwise = iterate rotateCounterClockwise matrix !! ((-n) `mod` 4)

-- ============================================
-- Задача 9: Триъгълник на Паскал като поток
-- ============================================

pascalTriangle :: [[Integer]]
pascalTriangle = iterate nextRow [1]
  where
    nextRow row = zipWith (+) (0 : row) (row ++ [0])

-- Обяснение: всеки следващ ред се получава като сумата на съседните елементи
-- от предишния ред. Добавяме 0 в началото и края, за да получим правилните суми.

-- ============================================
-- Тестови извиквания
-- ============================================

main :: IO ()
main = do
  putStrLn "=== Тестови извиквания за Упражнение 11 ===\n"

  -- Задача 1: Групиране по свойство
  putStrLn "=== Задача 1: Групиране по свойство ==="
  putStrLn $ "groupBy (`mod` 3) [1, 4, 2, 7, 5, 8, 3, 6, 9] = " ++ show (groupBy (`mod` 3) [1, 4, 2, 7, 5, 8, 3, 6, 9])
  putStrLn $ "groupBy length [\"hi\", \"hello\", \"yo\", \"world\", \"hey\"] = " ++ show (groupBy length ["hi", "hello", "yo", "world", "hey"])
  putStrLn $ "groupBy head [\"apple\", \"banana\", \"apricot\", \"blueberry\", \"avocado\"] = " ++ show (groupBy head ["apple", "banana", "apricot", "blueberry", "avocado"])
  putStrLn ""

  -- Задача 2: Прозорци със условие
  putStrLn "=== Задача 2: Прозорци със условие ==="
  putStrLn $ "conditionalWindows even 3 [1, 2, 3, 4, 5, 6, 7, 8] = " ++ show (conditionalWindows even 3 [1, 2, 3, 4, 5, 6, 7, 8])
  putStrLn $ "conditionalWindows (>3) 2 [1, 5, 2, 6, 3, 7] = " ++ show (conditionalWindows (> 3) 2 [1, 5, 2, 6, 3, 7])
  putStrLn $ "conditionalWindows odd 4 [2, 4, 6, 8] = " ++ show (conditionalWindows odd 4 [2, 4, 6, 8])
  putStrLn ""

  -- Задача 3: Вложени трансформации
  putStrLn "=== Задача 3: Вложени трансформации ==="
  putStrLn $ "nestedMap [(*2), (+10), (^2)] [1, 2] = " ++ show (nestedMap [(* 2), (+ 10), (^ 2)] [1, 2])
  putStrLn $ "nestedMap [(+1), (*3)] [5] = " ++ show (nestedMap [(+ 1), (* 3)] [5])
  putStrLn ""

  -- Задача 4: Потоци с памет
  putStrLn "=== Задача 4: Потоци с памет ==="
  putStrLn $ "take 10 (streamWithMemory (+) 1 0) = " ++ show (take 10 (streamWithMemory (+) 1 0))
  putStrLn $ "take 8 (streamWithMemory (*) 2 1) = " ++ show (take 8 (streamWithMemory (*) 2 1))
  putStrLn ""

  -- Задача 5: Сливане на сортирани списъци
  putStrLn "=== Задача 5: Сливане на сортирани списъци ==="
  putStrLn $ "mergeLists [[1, 4, 7], [2, 5, 8], [3, 6, 9]] = " ++ show (mergeLists [[1, 4, 7], [2, 5, 8], [3, 6, 9]])
  putStrLn $ "mergeLists [[1, 3, 5], [2, 4], [0, 10]] = " ++ show (mergeLists [[1, 3, 5], [2, 4], [0, 10]])
  putStrLn $ "mergeLists [[5], [], [1, 2, 3]] = " ++ show (mergeLists [[5 :: Int], [], [1, 2, 3]])
  putStrLn ""

  -- Задача 6: Матрична транспозиция с условие
  putStrLn "=== Задача 6: Матрична транспозиция с условие ==="
  putStrLn $ "selectiveTranspose even [[1, 2, 3], [4, 5, 6]] = " ++ show (selectiveTranspose even [[1, 2, 3], [4, 5, 6]])
  putStrLn $ "selectiveTranspose odd [[2, 4], [6, 8]] = " ++ show (selectiveTranspose odd [[2, 4], [6, 8]])
  putStrLn ""

  -- Задача 7: Сума в списък от списъци
  putStrLn "=== Задача 7: Сума в списък от списъци ==="
  putStrLn $ "deepSum even [[1, 2], [3, 4], [5]] = " ++ show (deepSum even [[1, 2], [3, 4], [5]])
  putStrLn $ "deepSum (>0) [[-5], [10, -3, 7], [2]] = " ++ show (deepSum (> 0) [[-5], [10, -3, 7], [2]])
  putStrLn $ "deepSum odd [[2], [4, 6], []] = " ++ show (deepSum odd [[2], [4, 6], []])
  putStrLn ""

  -- Задача 8: Матрично завъртане
  putStrLn "=== Задача 8: Матрично завъртане ==="
  putStrLn $ "rotateMatrix 1 [[1, 2, 3], [4, 5, 6]] = " ++ show (rotateMatrix 1 [[1, 2, 3], [4, 5, 6]])
  putStrLn $ "rotateMatrix 2 [[1, 2], [3, 4]] = " ++ show (rotateMatrix 2 [[1, 2], [3, 4]])
  putStrLn $ "rotateMatrix 4 [[1, 2], [3, 4]] = " ++ show (rotateMatrix 4 [[1, 2], [3, 4]])
  putStrLn $ "rotateMatrix (-1) [[1, 2, 3], [4, 5, 6]] = " ++ show (rotateMatrix (-1) [[1, 2, 3], [4, 5, 6]])
  putStrLn ""

  -- Задача 9: Триъгълник на Паскал като поток
  putStrLn "=== Задача 9: Триъгълник на Паскал като поток ==="
  putStrLn $ "take 5 pascalTriangle = " ++ show (take 5 pascalTriangle)
  putStrLn $ "take 7 pascalTriangle = " ++ show (take 7 pascalTriangle)
  putStrLn ""

  putStrLn "=== Край на тестовете ==="
