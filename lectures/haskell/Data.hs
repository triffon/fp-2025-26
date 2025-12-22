module Data where

-- >>> :t map
-- map :: (a -> b) -> [a] -> [b]
-- >>> :k Bool
-- Bool :: *
-- >>> 

-- >>> :k [[(Bool,Int)]]
-- [[(Bool,Int)]] :: *

type UnaryFunction a = a -> a

-- >>> :k UnaryFunction
-- UnaryFunction :: * -> *
-- >>> :k UnaryFunction Bool
-- UnaryFunction Bool :: *

type Dictionary k v = [(k,v)]
-- >>> :k Dictionary
-- Dictionary :: * -> * -> *
-- >>> :k Dictionary Char
-- Dictionary Char :: * -> *
-- >>> :k Dictionary Char String
-- Dictionary Char String :: *

-- >>> :t ()
-- () :: ()
-- >>> :k ()
-- () :: *
-- >>> :t []
-- [] :: [a]
-- >>> :k []
-- [] :: * -> *
-- >>> :k [] Int
-- [] Int :: *
-- >>> :k [Int]
-- [Int] :: *
-- >>> :k ([] ([] Int))
-- ([] ([] Int)) :: *
-- >>> :k 

-- >>> :t (,)
-- (,) :: a -> b -> (a, b)

-- >>> :k (,)
-- (,) :: * -> * -> *

-- >>> :k (,) Int Bool
-- (,) Int Bool :: *
-- (Int, Bool)

-- >>> :k (,) Int
-- (,) Int :: * -> *

-- >>> :k (->)
-- (->) :: * -> * -> *

-- >>> :k (->) Int
-- (->) Int :: * -> *

-- >>> :t 5
-- 5 :: Num a => a

-- >>> :t maxBound
-- maxBound :: Bounded a => a

-- >>> maxBound::Int
-- 9223372036854775807

-- >>> maxBound::Bool
-- True

-- >>> maxBound::Char
-- '\1114111'

-- >>> :t (+)
-- (+) :: Num a => a -> a -> a

-- >>> :t (/)
-- (/) :: Fractional a => a -> a -> a

-- >>> :t div
-- div :: Integral a => a -> a -> a

-- >>> :t (==)
-- (==) :: Eq a => a -> a -> Bool

-- >>> :t elem
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool

-- >>> :t show
-- show :: Show a => a -> String

-- >>> :k Num
-- Num :: * -> Constraint

class Measurable a where
    size :: a -> Integer
    empty :: a -> Bool
    -- empty x = size x == 0
    -- empty x = (0==) (size x)
    empty = (==0) . size

-- >>> :k Measurable
-- Measurable :: * -> Constraint

-- >>> :t size
-- size :: Measurable a => a -> Integer

-- >>> size [1..10]
-- No instance for `Measurable [Integer]' arising from a use of `size'
-- In the expression: size [1 .. 10]
-- In an equation for `it_aAOa': it_aAOa = size [1 .. 10]

-- >>> empty [1..10]
-- No instance for `Measurable [Integer]'
--   arising from a use of `empty'
-- In the expression: empty [1 .. 10]
-- In an equation for `it_aBxx': it_aBxx = empty [1 .. 10]

larger :: (Measurable a1, Measurable a2) => a1 -> a2 -> Bool
larger x y = size x > size y

instance Measurable Integer where
    size 0 = 0
    size n = 1 + size (div n 10)

-- >>> size 128347128937318237
-- 18

-- >>> empty 18127891478
-- False

instance (Measurable a, Measurable b) => Measurable (a, b) where
    size (x, y) = size x + size y

-- >>> size (8327128937,(812321,12312))
-- 21

instance Measurable a => Measurable [a] where
    -- size l = sum (map size l)
    size = sum . map size

-- >>> size [213,123,1551]
-- 10

-- "a", "aa", "aaa", "aaaa", ... 

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq, Ord, Enum, Read, Show)

today :: Weekday
today = Wed

-- >>> :t Tue
-- Tue :: Weekday

-- >>> :k Weekday
-- Weekday :: *

-- >>> today
-- Wed

-- >>> today == Thu
-- False

-- >>> Mon < Sun
-- True

-- >>> [Mon .. Fri]
-- [Mon,Tue,Wed,Thu,Fri]

x :: Weekday
x = read "Mon"

-- >>> x
-- Mon

data Unit = Unit

-- >>> :t Unit
-- Unit :: Unit

type Name = String
type Score = Int
-- data Player = Player Name Score
data Player = Player { name :: Name, score :: Score }
    deriving (Eq, Ord, Read, Show)

-- >>> :t Player
-- Player :: Name -> Score -> Player

-- >>> :k Player
-- Player :: *

katniss :: Player
-- katniss = Player "Katniss Everdeen" 45
katniss = Player { score = 45, name = "Katniss Everdeen" }

mario :: Player
mario = Player { score = 30, name = "Mario" }

-- >>> katniss < mario
-- True


-- >>> :t katniss
-- katniss :: Player

-- >>> katniss
-- Player {name = "Katniss Everdeen", score = 45}

-- >>> name katniss
-- "Katniss Everdeen"

-- >>> :t name
-- name :: Player -> Name

data Shape = Circle { radius :: Double } | Rectangle { width, height :: Double }
    deriving (Eq, Ord, Read, Show)

circle :: Shape
circle = Circle 2.3

-- >>> circle
-- Circle {radius = 2.3}

rectangle :: Shape
rectangle = Rectangle 5.8 3.5

-- >>> circle < rectangle
-- True

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

-- >>> area circle
-- 16.619025137490002

-- >>> area rectangle
-- 20.3

-- data MyPair a b = MyPair a b
