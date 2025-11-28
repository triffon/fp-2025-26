-- Решения на задачите от Упражнение 9: Функции от по-висок ред, IO, файлове

import Data.List
import Data.Ord (comparing)
import System.IO

-- ============================================
-- Секция 2: Анонимни функции
-- ============================================

-- Задача 2.1: Сума на (x^2 - 3x + 2) за положителни числа
transformSum :: [Int] -> Int
transformSum xs = sum (map (\x -> x ^ 2 - 3 * x + 2) (filter (> 0) xs))

-- ============================================
-- Секция 5: main и do нотация
-- ============================================

-- Задача 5.1: Чете списък от числа и извежда сумата
task51 :: IO ()
task51 = do
  line <- getLine
  let numbers = map read (words line) :: [Int]
  let sumVal = sum numbers
  putStrLn ("Sum: " ++ show sumVal)

-- Задача 5.2: Чете списък от числа и прилага трансформацията от 2.1
task52 :: IO ()
task52 = do
  line <- getLine
  let numbers = read line :: [Int]
  let result = transformSum numbers
  print result

-- ============================================
-- Секция 6: Работа с файлове
-- ============================================

-- Задача 6.1: Чете файл и извежда броя на редовете
task61 :: IO ()
task61 = do
  content <- readFile "input.txt"
  let lineCount = length (lines content)
  putStrLn ("Number of lines: " ++ show lineCount)

-- Задача 6.2: Чете файл и записва редовете в обратен ред
task62 :: IO ()
task62 = do
  content <- readFile "input.txt"
  let reversedLines = reverse (lines content)
  writeFile "output.txt" (unlines reversedLines)

-- ============================================
-- Секция 7: Комбинирани задачи
-- ============================================

-- Задача 7.1: Обработка на файл с специфична структура
task71 :: IO ()
task71 = do
  putStr "Enter filename: "
  name <- getLine
  content <- readFile name
  -- Използваме pattern matching за да извлечем първите 3 реда
  -- Забележка: Ако файлът няма 3 реда, това ще хвърли грешка.
  let (nLine : firstLine : secondLine : _) = lines content
      n = read nLine :: Int
      first = read firstLine :: [Int]
      second = read secondLine :: [Int]
      result = zipWith (\f s -> f ^ n + s * n) first second
      outputFilename = name ++ "-output.txt"
  writeFile outputFilename (show result)

-- Задача 7.2: Статистики от числа във файл
-- Помощна функция за мода
mode :: [Int] -> Int
mode xs = fst (maximumBy (\a b -> compare (snd a) (snd b)) counts)
  where
    counts = map (\x -> (x, length (filter (== x) xs))) (nub xs)

task72 :: IO ()
task72 = do
  putStr "Enter filename: "
  name <- getLine
  content <- readFile name
  let numbers = read content :: [Int]
      sumVal = sum numbers
      avg = fromIntegral sumVal / fromIntegral (length numbers)
      modeVal = mode numbers
      minVal = minimum numbers
      maxVal = maximum numbers
      outputFilename = name ++ "-statistics.txt"
      output =
        unlines
          [ "Sum: " ++ show sumVal,
            "Average: " ++ show avg,
            "Mode: " ++ show modeVal,
            "Min: " ++ show minVal,
            "Max: " ++ show maxVal
          ]
  writeFile outputFilename output

-- Задача 7.3: Премахване на думи с дължина < 4
task73 :: IO ()
task73 = do
  putStr "Enter input filename: "
  inputName <- getLine
  putStr "Enter output filename: "
  outputName <- getLine
  content <- readFile inputName
  let wordsList = words content
      filteredWords = filter (\w -> length w >= 4) wordsList
      result = unwords filteredWords
  writeFile outputName result

-- Задача 7.4: Прости числа от файл
-- Помощна функция за проверка дали число е просто
isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = null [x | x <- [3, 5 .. floor (sqrt (fromIntegral n))], n `mod` x == 0]

task74 :: IO ()
task74 = do
  putStr "Enter input filename: "
  inputName <- getLine
  putStr "Enter output filename: "
  outputName <- getLine
  content <- readFile inputName
  let numbers = read content :: [Int]
      primes = filter isPrime numbers
      sortedPrimes = sort primes
  writeFile outputName (show sortedPrimes)

-- Задача 7.5: Брой срещания на думи
-- Помощна функция за броене на срещания
countOccurrences :: [String] -> [(String, Int)]
countOccurrences words = map (\w -> (w, length (filter (== w) words))) (nub words)

-- TODO: ПРОМЯНААААА
-- TODO: Да добавя истински тестове може би?
-- Помощна функция за сортиране по брой срещания (най-често срещаните първо)
sortByCount :: [(String, Int)] -> [(String, Int)]
sortByCount xs = reverse (sortOn snd xs)

task75 :: IO ()
task75 = do
  putStr "Enter input filename: "
  inputName <- getLine
  putStr "Enter output filename: "
  outputName <- getLine
  content <- readFile inputName
  let wordsList = words content
      wordCounts = countOccurrences wordsList
      sortedCounts = sortByCount wordCounts
      output = unlines (map (\(w, c) -> w ++ ": " ++ show c) sortedCounts)
  writeFile outputName output

-- Задача 7.6: Подсписъци с максимална сума
-- Помощна функция за генериране на всички подсписъци с дължина n
subsequencesOfLength :: Int -> [a] -> [[a]]
subsequencesOfLength n xs
  | n > length xs = []
  | n <= 0 = [[]]
  | otherwise = [take n (drop i xs) | i <- [0 .. length xs - n]]

task76 :: IO ()
task76 = do
  putStr "Enter input filename: "
  inputName <- getLine
  putStr "Enter n: "
  nStr <- getLine
  let n = read nStr :: Int
  content <- readFile inputName
  let numbers = read content :: [Int]
      allSubseqs = subsequencesOfLength n numbers
      sums = map sum allSubseqs
      maxSum = maximum sums
      maxSubseqs = filter (\sub -> sum sub == maxSum) allSubseqs
      outputName = inputName ++ "-output.txt"
  writeFile outputName (show maxSubseqs)

-- ============================================
-- Тестови извиквания
-- ============================================
main :: IO ()
main = do
  putStrLn "=== Тестови извиквания ===\n"

  -- Секция 2: Анонимни функции
  putStrLn "=== Секция 2: Анонимни функции ==="
  putStrLn "Задача 2.1: transformSum"
  putStrLn ("transformSum [-3, 4, -1, 2, 5, -3, 7] = " ++ show (transformSum [-3, 4, -1, 2, 5, -3, 7]))
  putStrLn ("transformSum [-3, -2, -1] = " ++ show (transformSum [-3, -2, -1]))
  putStrLn ("transformSum [1, 2] = " ++ show (transformSum [1, 2]))
  putStrLn ""

  -- Секция 5: main и do нотация
  putStrLn "=== Секция 5: main и do нотация ==="
  putStrLn "Задача 5.1: task51"
  putStrLn "За да тествате, стартирайте: task51"
  putStrLn "Въведете: 1 2 3 4 5"
  putStrLn ""

  putStrLn "Задача 5.2: task52"
  putStrLn "За да тествате, стартирайте: task52"
  putStrLn "Въведете: [-3, 4, -1, 2, 5, -3, 7]"
  putStrLn ""

  -- Секция 6: Работа с файлове
  putStrLn "=== Секция 6: Работа с файлове ==="
  putStrLn "Задача 6.1: task61"
  putStrLn "За да тествате, създайте файл 'input.txt' и стартирайте: task61"
  putStrLn ""

  putStrLn "Задача 6.2: task62"
  putStrLn "За да тествате, създайте файл 'input.txt' и стартирайте: task62"
  putStrLn "Резултатът ще бъде записан в 'output.txt'"
  putStrLn ""

  -- Секция 7: Комбинирани задачи
  putStrLn "=== Секция 7: Комбинирани задачи ==="
  putStrLn "Задача 7.1: task71"
  putStrLn "За да тествате, създайте файл с 3 реда:"
  putStrLn "  2"
  putStrLn "  [1, 2, 3]"
  putStrLn "  [4, 5, 6]"
  putStrLn "И стартирайте: task71"
  putStrLn ""

  putStrLn "Задача 7.2: task72"
  putStrLn "За да тествате, създайте файл с списък от числа, напр.: [1, 2, 2, 3, 3, 3, 4]"
  putStrLn "И стартирайте: task72"
  putStrLn ""

  putStrLn "Задача 7.3: task73"
  putStrLn "За да тествате, създайте файл с текст и стартирайте: task73"
  putStrLn ""

  putStrLn "Задача 7.4: task74"
  putStrLn "За да тествате, създайте файл с списък от числа, напр.: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
  putStrLn "И стартирайте: task74"
  putStrLn ""

  putStrLn "Задача 7.5: task75"
  putStrLn "За да тествате, създайте файл с текст и стартирайте: task75"
  putStrLn ""

  putStrLn "Задача 7.6: task76"
  putStrLn "За да тествате, създайте файл с списък от числа, напр.: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
  putStrLn "И стартирайте: task76"
  putStrLn "Въведете n (например 3)"
  putStrLn ""

  putStrLn "=== Край на тестовете ==="
