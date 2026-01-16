{-# LANGUAGE InstanceSigs #-}

{- HLINT ignore "Use tuple-section" -}

-- 🎯 State монадата - Пълно обяснение и решения
-- ================================================

-- 1. Основната дефиниция
-- =======================

-- State s r е "обвивка" около функция
-- s е типът на състоянието (например Int за баланс)
-- r е типът на резултата (например Bool за успех/неуспех)
-- runState е функция, която извлича вътрешната функция

newtype State s r = State {runState :: s -> (r, s)}

-- Примери за GHCi:
-- ghci> let simple = State $ \s -> (s * 2, s + 1)
-- ghci> runState simple 10
-- (20,11)  -- резултат: 10*2=20, ново състояние: 10+1=11

-- ghci> let ignoreState = State $ \s -> (42, s)
-- ghci> runState ignoreState 100
-- (42,100)  -- резултат: 42, състоянието не се променя

-- ghci> let changeState = State $ \s -> ((), s * 3)
-- ghci> runState changeState 5
-- ((),15)  -- резултат: (), ново състояние: 5*3=15

-- 2. Functor инстанцията (Задача 8.1)
-- ====================================

-- Какво прави:
-- fmap трансформира резултата, без да пипа състоянието
-- Изпълнява оригиналното изчисление g
-- Прилага функция f върху резултата
-- Състоянието преминава непроменено

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s ->
    let (a, newS) = g s
     in (f a, newS)

-- Примери за GHCi:
-- ghci> let st = State $ \s -> (10, s + 1)
-- ghci> runState st 5
-- (10,6)

-- ghci> runState (fmap (*2) st) 5
-- (20,6)  -- резултатът е удвоен (10*2=20), състоянието си е същото

-- ghci> runState (fmap (+5) st) 5
-- (15,6)  -- резултат: 10+5=15, състояние: 5+1=6

-- ghci> runState (fmap (*2) (fmap (+1) st)) 5
-- (22,6)  -- (10 + 1) * 2 = 22

-- 3. Applicative инстанцията (Задача 8.2)
-- ========================================

-- Какво прави:
-- pure a - създава State, който връща a без да променя състоянието
-- <*> - комбинира две изчисления последователно:
--   1. Изпълнява първото → получава функция и ново състояние
--   2. Изпълнява второто с новото състояние → получава стойност
--   3. Прилага функцията върху стойността

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State f) <*> (State g) = State $ \s ->
    let (func, s') = f s
        (a, s'') = g s'
     in (func a, s'')

-- Примери за GHCi:
-- ghci> runState (pure 42) 10
-- (42,10)  -- връща 42, състоянието си е 10

-- ghci> let f = State $ \s -> ((+10), s * 2)  -- връща функция (+10)
-- ghci> let g = State $ \s -> (5, s + 3)       -- връща 5
-- ghci> runState (f <*> g) 10
-- (15,23)  -- (10*2) + 3 = 23, резултат: 5+10 = 15

-- Стъпка по стъпка:
-- 1. f с състояние 10: получаваме ((+10), 20)
-- 2. g с състояние 20: получаваме (5, 23)
-- 3. Прилагаме (+10) на 5: получаваме 15
-- Финално: (15, 23)

-- ghci> runState (pure (+) <*> pure 3 <*> pure 5) 0
-- (8,0)  -- 3 + 5 = 8, състоянието не се променя

-- 4. Monad инстанцията ⭐ (НАЙ-ВАЖНО) (Задача 8.3)
-- =================================================

-- Какво прави:
-- return = pure (обгръща стойност)
-- >>= (bind) - chain-ва изчисления, които зависят едно от друго:
--   1. Изпълнява първото изчисление g с текущото състояние
--   2. Получава резултат a и ново състояние newS
--   3. Подава резултата a на функцията f → получава ново изчисление
--   4. Изпълнява новото изчисление с новото състояние newS

instance Monad (State s) where
  return :: a -> State s a
  return a = State $ \s -> (a, s)

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State g) >>= f = State $ \s ->
    let (a, newS) = g s
     in runState (f a) newS

-- Примери за GHCi:
-- ghci> let step1 = State $ \s -> (s * 2, s + 1)  -- връща удвоено състояние
-- ghci> let step2 x = State $ \s -> (x + s, s * 2) -- използва резултата от step1
-- ghci> runState (step1 >>= step2) 10
-- (31,22)

-- Стъпка по стъпка:
-- 1. step1 с 10: (20, 11)  -- резултат=20, състояние=11
-- 2. step2 20 с 11: (31, 22) -- резултат=20+11=31, състояние=11*2=22

-- ghci> let addToState x = State $ \s -> (s + x, s)
-- ghci> let computation = addToState 5 >>= \r -> addToState (r * 2)
-- ghci> runState computation 10
-- (30,10)  -- (10+5)*2 = 30

-- ghci> let c = return 100 >>= \x -> return (x + 20) >>= \y -> return (y * 2)
-- ghci> runState c 0
-- (240,0)  -- (100 + 20) * 2 = 240

-- 5. Базовите операции
-- =====================

-- get - чете състоянието (Задача 8.4)
-- ===================================

-- get връща състоянието като резултат

get :: State s s
get = State $ \s -> (s, s)

-- Примери:
-- ghci> runState get 42
-- (42,42)  -- резултат: 42, състояние: 42 (непроменено)

-- ghci> runState (get >>= \x -> return (x * 2)) 10
-- (20,10)  -- прочита 10, връща 20, състоянието си е 10

-- put - задава ново състояние (Задача 8.5)
-- =========================================

-- put игнорира старото състояние и задава ново

put :: s -> State s ()
put s = State (const ((), s))

-- Примери:
-- ghci> runState (put 100) 42
-- ((),100)  -- резултат: (), ново състояние: 100

-- ghci> runState (put 50 >> get) 10
-- (50,50)  -- задава 50, после го чете

-- ghci> runState (get >>= \x -> put (x * 2) >> get) 10
-- (20,20)  -- чете 10, задава 20, връща 20

-- modify - модифицира състоянието (Задача 8.6)
-- =============================================

-- modify прилага функция върху състоянието

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- Примери:
-- ghci> runState (modify (+10)) 5
-- ((),15)  -- добавя 10 към 5

-- ghci> runState (modify (*3)) 7
-- ((),21)  -- умножава по 3

-- ghci> runState (modify (+10) >> modify (*2)) 5
-- ((),30)  -- (5 + 10) * 2 = 30

-- ghci> runState (modify (+5) >> get >>= \x -> return (x * 2)) 10
-- (30,15)  -- състояние става 15, връща 30

-- 6. Задача 6.1: safeSqrt
-- ========================

-- safeSqrt връща квадратен корен само за неотрицателни числа

safeSqrt :: Double -> Maybe Double
safeSqrt x
  | x < 0 = Nothing
  | otherwise = Just (sqrt x)

-- Примери:
-- ghci> safeSqrt 16
-- Just 4.0
-- ghci> safeSqrt 9
-- Just 3.0
-- ghci> safeSqrt 0
-- Just 0.0
-- ghci> safeSqrt (-1)
-- Nothing
-- ghci> safeSqrt (-25)
-- Nothing

-- 7. Банковите операции
-- ======================

-- deposit (Задача 9.1)
-- ====================

-- deposit добавя пари към банкова сметка

deposit :: Int -> State Int ()
deposit amount = modify (+ amount)

-- Примери:
-- ghci> runState (deposit 100) 0
-- ((),100)  -- депозира 100, баланс: 0 → 100

-- ghci> runState (deposit 50) 100
-- ((),150)  -- депозира 50, баланс: 100 → 150

-- ghci> runState (deposit 10 >> deposit 20 >> deposit 30) 0
-- ((),60)  -- 0 + 10 + 20 + 30 = 60

-- withdraw (Задача 9.2)
-- ======================

-- withdraw опитва да изтегли пари от банкова сметка
-- Връща True ако успее, False ако няма достатъчно пари

withdraw :: Int -> State Int Bool
withdraw amount = do
  balance <- get
  if balance < amount
    then return False
    else True <$ modify (subtract amount)

-- Примери:
-- ghci> runState (withdraw 30) 100
-- (True,70)  -- успех, баланс: 100 → 70

-- ghci> runState (withdraw 150) 100
-- (False,100)  -- неуспех, балансът не се променя

-- ghci> runState (deposit 100 >> withdraw 30) 0
-- (True,70)  -- депозира 100, тегли 30 → 70

-- ghci> runState (deposit 50 >> withdraw 100) 0
-- (False,50)  -- депозира 50, опит за 100 → неуспех, остава 50

-- getBalance (Задача 9.4)
-- ========================

-- getBalance връща текущия баланс

getBalance :: State Int Int
getBalance = get

-- Пример:
-- ghci> runState getBalance 100
-- (100,100)

-- session - пълна банкова сесия (Задача 9.3)
-- ===========================================

session :: State Int String
session = do
  deposit 100 -- баланс: 0 → 100
  ok1 <- withdraw 30 -- баланс: 100 → 70, ok1 = True
  deposit 50 -- баланс: 70 → 120
  ok2 <- withdraw 150 -- баланс: 120 (няма достатъчно), ok2 = False
  deposit 80 -- баланс: 120 → 200
  return $ "Withdraw 30: " ++ show ok1 ++ ", Withdraw 150: " ++ show ok2

-- Тест:
-- ghci> runState session 0
-- ("Withdraw 30: True, Withdraw 150: False",200)

-- Стъпка по стъпка:
-- 1. deposit 100:    баланс = 100
-- 2. withdraw 30:    баланс = 70, ok1 = True
-- 3. deposit 50:     баланс = 120
-- 4. withdraw 150:   баланс = 120, ok2 = False (няма достатъчно!)
-- 5. deposit 80:     баланс = 200
-- 6. return string:  резултат със статуса на двете теглития

-- 8. Задача 11.1: calculateFromFile
-- ==================================

-- calculateFromFile чете файл с транзакции и връща:
-- 1. Крайна наличност
-- 2. Сумарно депозити
-- 3. Сумарно тегления

-- Тип за проследяване на статистика
type BankStats = (Int, Int, Int) -- (баланс, депозити, тегления)

-- Парсване на една операция
parseOperation :: String -> Maybe (State BankStats ())
parseOperation line =
  case words line of
    ["deposit", amountStr] ->
      case reads amountStr of
        [(amount, "")] -> Just $ depositWithStats amount
        _ -> Nothing
    ["withdraw", amountStr] ->
      case reads amountStr of
        [(amount, "")] -> Just $ withdrawWithStats amount
        _ -> Nothing
    _ -> Nothing

-- deposit със статистика
depositWithStats :: Int -> State BankStats ()
depositWithStats amount = modify $ \(balance, totalDeposits, totalWithdraws) ->
  (balance + amount, totalDeposits + amount, totalWithdraws)

-- withdraw със статистика
withdrawWithStats :: Int -> State BankStats ()
withdrawWithStats amount = do
  (balance, totalDeposits, totalWithdraws) <- get
  if balance >= amount
    then put (balance - amount, totalDeposits, totalWithdraws + amount)
    else return () -- неуспешно теглене, не променяме нищо

calculateFromFile :: FilePath -> IO (Int, Int, Int)
calculateFromFile path = do
  contents <- readFile path
  let fileLines = lines contents
  case fileLines of
    [] -> return (0, 0, 0)
    (firstLine : rest) ->
      case reads firstLine of
        [(initialBalance, "")] -> do
          let operations = [op | line <- rest, Just op <- [parseOperation line]]
          let finalStats = runState (sequence_ operations) (initialBalance, 0, 0)
          return (snd finalStats)
        _ -> return (0, 0, 0)

-- 9. Main функция с тестове
-- ==========================

main :: IO ()
main = do
  -- Тест 1: safeSqrt
  putStrLn "Задача 6.1: safeSqrt"
  putStrLn "------------------------"
  putStrLn $ "safeSqrt 16 = " ++ show (safeSqrt 16)
  putStrLn $ "safeSqrt 9 = " ++ show (safeSqrt 9)
  putStrLn $ "safeSqrt 0 = " ++ show (safeSqrt 0)
  putStrLn $ "safeSqrt (-1) = " ++ show (safeSqrt (-1))
  putStrLn $ "safeSqrt (-25) = " ++ show (safeSqrt (-25))
  putStrLn ""

  -- Тест 2: Functor
  putStrLn "Задача 8.1: Functor инстанция"
  putStrLn "--------------------------------"
  let st = State $ \s -> (10, s + 1)
  putStrLn $ "runState st 5 = " ++ show (runState st 5)
  putStrLn $ "runState (fmap (*2) st) 5 = " ++ show (runState (fmap (* 2) st) 5)
  putStrLn $ "runState (fmap (+5) st) 5 = " ++ show (runState (fmap (+ 5) st) 5)
  putStrLn ""

  -- Тест 3: Applicative
  putStrLn "Задача 8.2: Applicative инстанция"
  putStrLn "------------------------------------"
  putStrLn $ "runState (pure 42) 10 = " ++ show (runState (pure 42) 10)
  let f = State $ \s -> ((+ 10), s * 2)
  let g = State $ \s -> (5, s + 3)
  putStrLn $ "runState (f <*> g) 10 = " ++ show (runState (f <*> g) 10)
  putStrLn ""

  -- Тест 4: Monad
  putStrLn "Задача 8.3: Monad инстанция"
  putStrLn "------------------------------"
  let step1 = State $ \s -> (s * 2, s + 1)
  let step2 x = State $ \s -> (x + s, s * 2)
  putStrLn $ "runState (step1 >>= step2) 10 = " ++ show (runState (step1 >>= step2) 10)
  putStrLn ""

  -- Тест 5: get
  putStrLn "Задача 8.4: get"
  putStrLn "------------------"
  putStrLn $ "runState get 42 = " ++ show (runState get 42)
  putStrLn $ "runState (get >>= \\x -> return (x * 2)) 10 = " ++ show (runState (get >>= \x -> return (x * 2)) 10)
  putStrLn ""

  -- Тест 6: put
  putStrLn "Задача 8.5: put"
  putStrLn "------------------"
  putStrLn $ "runState (put 100) 42 = " ++ show (runState (put 100) 42)
  putStrLn $ "runState (put 50 >> get) 10 = " ++ show (runState (put 50 >> get) 10)
  putStrLn ""

  -- Тест 7: modify
  putStrLn "Задача 8.6: modify"
  putStrLn "---------------------"
  putStrLn $ "runState (modify (+10)) 5 = " ++ show (runState (modify (+ 10)) 5)
  putStrLn $ "runState (modify (*3)) 7 = " ++ show (runState (modify (* 3)) 7)
  putStrLn $ "runState (modify (+10) >> modify (*2)) 5 = " ++ show (runState (modify (+ 10) >> modify (* 2)) 5)
  putStrLn ""

  -- Тест 8: deposit
  putStrLn "Задача 9.1: deposit"
  putStrLn "----------------------"
  putStrLn $ "runState (deposit 100) 0 = " ++ show (runState (deposit 100) 0)
  putStrLn $ "runState (deposit 50) 100 = " ++ show (runState (deposit 50) 100)
  putStrLn $ "runState (deposit 10 >> deposit 20 >> deposit 30) 0 = " ++ show (runState (deposit 10 >> deposit 20 >> deposit 30) 0)
  putStrLn ""

  -- Тест 9: withdraw
  putStrLn "Задача 9.2: withdraw"
  putStrLn "-----------------------"
  putStrLn $ "runState (withdraw 30) 100 = " ++ show (runState (withdraw 30) 100)
  putStrLn $ "runState (withdraw 150) 100 = " ++ show (runState (withdraw 150) 100)
  putStrLn $ "runState (deposit 100 >> withdraw 30) 0 = " ++ show (runState (deposit 100 >> withdraw 30) 0)
  putStrLn $ "runState (deposit 50 >> withdraw 100) 0 = " ++ show (runState (deposit 50 >> withdraw 100) 0)
  putStrLn ""

  -- Тест 10: getBalance
  putStrLn "Задача 9.4: getBalance"
  putStrLn "-------------------------"
  putStrLn $ "runState getBalance 100 = " ++ show (runState getBalance 100)
  let checkBalance = do
        deposit 50
        balance <- getBalance
        withdraw 20
        return balance
  putStrLn $ "runState checkBalance 0 = " ++ show (runState checkBalance 0)
  putStrLn ""

  -- Тест 11: session
  putStrLn "Задача 9.3: session (пълна банкова сесия)"
  putStrLn "--------------------------------------------"
  putStrLn $ "runState session 0 = " ++ show (runState session 0)
  putStrLn ""
  putStrLn "Стъпка по стъпка:"
  putStrLn "1. deposit 100:    баланс = 100"
  putStrLn "2. withdraw 30:    баланс = 70, ok1 = True"
  putStrLn "3. deposit 50:     баланс = 120"
  putStrLn "4. withdraw 150:   баланс = 120, ok2 = False (няма достатъчно!)"
  putStrLn "5. deposit 80:     баланс = 200"
  putStrLn "6. return string:  резултат със статуса на двете теглития"
  putStrLn ""

  -- Тест 12: calculateFromFile
  putStrLn "Задача 11.1: calculateFromFile"
  putStrLn "----------------------------------"
  putStrLn "Тествам с файл transactions.txt:"
  putStrLn "50"
  putStrLn "deposit 100"
  putStrLn "withdraw 30"
  putStrLn "deposit 50"
  putStrLn "withdraw 200"
  putStrLn "deposit 25"
  putStrLn "withdraw 40"
  putStrLn ""
  result <- calculateFromFile "transactions.txt"
  putStrLn $ "calculateFromFile \"transactions.txt\" = " ++ show result
  putStrLn "  ^(155 крайна наличност, 175 депозити, 70 тегления)"
  putStrLn ""
  putStrLn "Обяснение:"
  putStrLn "1. Започваме с 50"
  putStrLn "2. Депозираме 100 → баланс = 150"
  putStrLn "3. Теглим 30 → баланс = 120"
  putStrLn "4. Депозираме 50 → баланс = 170"
  putStrLn "5. Теглим 200 (неуспех!) → баланс = 170"
  putStrLn "6. Депозираме 25 → баланс = 195"
  putStrLn "7. Теглим 40 → баланс = 155"
  putStrLn ""

-- 🎮 Експериментирай в GHCi:
-- ==========================

-- Напиши си собствена сесия:
-- ghci> let mySession = deposit 1000 >> withdraw 250 >> deposit 500 >> withdraw 2000
-- ghci> runState mySession 0
-- Какво ще е резултатът?

-- Още експерименти:
-- ghci> runState (get >>= \x -> put (x + 10) >> get >>= \y -> return (y * 2)) 5
-- Опитай да проследиш!

-- Проверка на баланс:
-- ghci> let checkAndDeposit = get >>= \balance -> if balance < 100 then deposit 100 else return ()
-- ghci> runState checkAndDeposit 50
-- ((),150)  -- имаше 50, добавя 100
-- ghci> runState checkAndDeposit 150
-- ((),150)  -- имаше 150, не добавя нищо

-- Ключови идеи:
-- =============
-- 1. State е функция от s -> (r, s) - прие състояние, върни резултат + ново състояние
-- 2. >>= chain-ва изчисления - резултатът от едното влиза в другото
-- 3. Състоянието "тече" през операциите - всяка следваща получава новото състояние
-- 4. do-notation е синтактична захар - прави chain-ването четимо
