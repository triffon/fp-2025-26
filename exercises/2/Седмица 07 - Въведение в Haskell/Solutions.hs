import Prelude hiding (repeat)

factorial :: Int -> Int
factorial n = if n == 0 then 1 else n * factorial (n - 1)

-- >>> factorial 5
-- 120

factorial' :: Int -> Int
factorial' n
  | n == 0 = 1
  | otherwise = n * factorial' (n - 1)

factorial'' :: Int -> Int
factorial'' 0 = 1
factorial'' n = n * factorial'' (n - 1)

-- roots' :: (Double, Double, Double) -> Double
-- roots' (a,b,c)

roots :: Double -> Double -> Double -> Double
roots a b c
  | discriminant a b c < 0 = 0
  | discriminant a b c == 0 = 1
  | otherwise = 2
  where
    discriminant :: Double -> Double -> Double -> Double
    discriminant a b c = b ** 2 - 4 * a * c

repeat :: (a -> a) -> Int -> a -> a
repeat f 0 x = x
-- repeat f 0 = \x -> x
-- repeat _ 0 = id
repeat f n x = f (repeat f (n - 1) x)

modulus :: (Double, Double) -> Double
-- modulus (real, imaginary) = sqrt (real ** 2 + imaginary ** 2)
modulus c = let real = fst c
                imaginary = snd c
            in sqrt (real ** 2 + imaginary ** 2)

(~=) :: Double -> Double -> Bool
a ~= b = let epsilon = 1e-6
  in abs (a - b) < epsilon

compute :: (Int, Int, Double) -> Double
compute (0, _, _) = 0
compute (x, y, z) = z / fromIntegral x + fromIntegral y