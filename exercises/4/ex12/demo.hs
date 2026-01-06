data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }

data Color = Red | Green | Blue

-- Функция, която работи с нашия тип
colorName :: Color -> String
colorName Red = "The color of blood"
colorName Green = "The color of grass"
colorName Blue = "The color of the sky"

data Point = Point2D Double Double
  deriving (Show, Eq)

-- Shape може да бъде Circle (с център и радиус) или Rectangle (с два срещуположни ъгъла)
data Shape
  = Circle Point Double -- Център, Радиус
  | Rectangle Point Point -- Долен ляв, Горен десен
  deriving (Show, Eq)

area :: Shape -> Double
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point2D x1 y1) (Point2D x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

data Animal = Cat | Dog | Mouse
  deriving (Show, Eq)

data TrafficLight = RedLight | YellowLight | GreenLight
  -- Искаме Eq да работи стандартно
  deriving (Eq)

-- Но искаме Show да извежда нещо специално на български
instance Show TrafficLight where
  show RedLight = "Stop!"
  show YellowLight = "Ready..."
  show GreenLight = "Go!"
