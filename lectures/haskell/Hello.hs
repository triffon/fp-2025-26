module Hello where

-- import Prelude hiding ((^))
import Data.List ( nub )

x :: Integer
x = 200000000000000000000000000000000000

y = 2.3

-- z = z

a :: Int
a = 2

-- >>> b
-- 7.4
b :: Double
b = fromIntegral a^2 + 3.4

-- >>> c
-- [1,2,3]
c = nub [1, 2, 3, 1, 2, 2]

-- >>> :t (+)
-- (+) :: Num a => a -> a -> a

-- >>> :t (^)
-- (^) :: (Num a, Integral b) => a -> b -> a

-- >>> :t (^^)
-- (^^) :: (Fractional a, Integral b) => a -> b -> a

-- >>> :t (**)
-- (**) :: Floating a => a -> a -> a

square :: Int -> Int
square x = x * x

-- >>> square(((((2)))))
-- 4

twice :: (t -> t) -> t -> t
twice f x = f (f x)

diag :: (t1 -> t1 -> t2) -> t1 -> t2
diag f x = f x x

-- >>> ((-)5) 8
-- -3

-- >>> (subtract 5) 8
-- 3

-- >>> (5-) 3
-- 2


fact :: Integer -> Integer
fact n
  | n == 0        = 1
  | n > 0         = n * fact (n - 1)
--  | otherwise     = fact (-n)
  | otherwise     = error "Факториел от отрицателно число!"

-- >>> fact (-5)
-- Факториел от отрицателно число!

-- >>> :t error
-- error :: HasCallStack => [Char] -> a
