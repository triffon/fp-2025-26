module Monad where

import Prelude hiding (Monad, return, (>>=), (>>))

class Applicative m => Monad m where
    return :: a -> m a
    return = pure

    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    x >> y = x >>= (\_ -> y)

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x >>= f  = f x

