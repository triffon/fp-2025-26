module Monad where

import Control.Applicative
import Prelude hiding (Monad, return, (>>=), (>>), (=<<))

class Applicative m => Monad m where
    return :: a -> m a
    return = pure

    (>>=) :: m a -> (a -> m b) -> m b
    (>>=) = flip (=<<)
    (>>) :: m a -> m b -> m b
    x >> y = x >>= (\_ -> y)

    (=<<) :: (a -> m b) -> m a -> m b
    (=<<) = flip (>>=)

instance Monad Maybe where
   -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  Just x >>= f  = f x

-- >>> do x <- Just 2; y <- Just $ x + 3; return y
-- Just 5

-- >>> Just 2 >>= (\x -> Just $ x + 3) >>= return
-- Just 5

instance Monad [] where
    
    -- (>>=) :: [a] -> (a -> [b]) -> [b]
   -- xs >>= f = concatMap f xs
   (=<<) = concatMap

-- allPythagoreanTriples = [ (x, y, z) | z <- [1..], y <- [1..z-1], x <- [1..y-1],
--                                       x^2 + y^2 == z^2, gcd x y == 1 ]

guard :: Alternative f => Bool -> f ()
guard False = empty
guard True = pure ()

allPythagoreanTriples = do z <- [1..]
                           y <- [1..z-1]
                           x <- [1..y-1]
                           guard $ x^2 + y^2 == z^2
                           guard $ gcd x y == 1
                           return (x, y, z)

-- >>> take 10 allPythagoreanTriples
-- [(3,4,5),(5,12,13),(8,15,17),(7,24,25),(20,21,29),(12,35,37),(9,40,41),(28,45,53),(11,60,61),(33,56,65)]
