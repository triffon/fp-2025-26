module Solutions where

-- Задача 1: Алгебрични типове данни (ADTs)
-- Дефиниция на тип Точка
data Point2D = Point2D Double Double
  deriving (Show, Eq)

-- Дефиниция на тип Фигура
data Shape
  = Circle Point2D Double -- Център, Радиус
  | Rectangle Point2D Point2D -- Долен ляв, Горен десен
  | Triangle Point2D Point2D Point2D -- Три точки
  deriving (Eq) -- Не извеждаме автоматично Show, защото ще го имплементираме ръчно в Задача 2

-- Помощна функция за разстояние между две точки
distance :: Point2D -> Point2D -> Double
distance (Point2D x1 y1) (Point2D x2 y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- Функция за пресмятане на лице
area :: Shape -> Double
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point2D x1 y1) (Point2D x2 y2)) = abs (x2 - x1) * abs (y2 - y1)
area (Triangle p1 p2 p3) =
  let a = distance p1 p2
      b = distance p2 p3
      c = distance p3 p1
      p = (a + b + c) / 2
   in sqrt (p * (p - a) * (p - b) * (p - c))

-- Задача 2: Типови класове
-- Ръчна инстанция за Show
instance Show Shape where
  show (Circle _ r) = "Circle with radius " ++ show r
  show (Rectangle p1 p2) = "Rectangle with points " ++ show p1 ++ " and " ++ show p2
  show (Triangle p1 p2 p3) = "Triangle with points " ++ show p1 ++ ", " ++ show p2 ++ " and " ++ show p3

-- Задача 3: Параметричен полиморфизъм
-- Дефиниция на тип MyList (наш собствен списък)
data MyList a = EmptyList | Cons a (MyList a)
  deriving (Show, Eq) -- Deriving Show за лесно визуализиране на резултатите

-- Функция map за нашия списък
myMap :: (a -> b) -> MyList a -> MyList b
myMap _ EmptyList = EmptyList
myMap f (Cons x xs) = Cons (f x) (myMap f xs)

-- Примери и тестване
main :: IO ()
main = do
  putStrLn "--- Задача 1 & 2: Фигури ---"
  let p1 = Point2D 0 0
  let p2 = Point2D 3 0
  let p3 = Point2D 0 4
  let c = Circle p1 5
  let r = Rectangle p1 (Point2D 10 20)
  let t = Triangle p1 p2 p3

  print c
  putStrLn $ "Area: " ++ show (area c)

  print r
  putStrLn $ "Area: " ++ show (area r)

  print t
  putStrLn $ "Area: " ++ show (area t)

  putStrLn "\n--- Задача 3: MyList ---"
  let list = Cons 1 (Cons 2 (Cons 3 EmptyList))
  print list
  let mappedList = myMap (* 2) list
  print mappedList
