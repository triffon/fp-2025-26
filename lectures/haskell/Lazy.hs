module Lazy where

-- >>> x
-- Prelude.head: empty list
x :: (Integer, Double)
x = (2, head [])

-- >>> fst x
-- 2

-- >>> snd x
-- Prelude.head: empty list

twos :: [Integer]
twos = 2 : twos

-- >>> head y
-- 2

-- >>> take 10 twos
-- [2,2,2,2,2,2,2,2,2,2]

nats = iterate (+1) 0

-- >>> take 10 nats
-- [0,1,2,3,4,5,6,7,8,9]
