{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import Prelude hiding (Maybe(..))

data Maybe a = Nothing | Just a deriving Show

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  (Just f) <*> (Just a) = Just $ f a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f fa fb = f <$> fa <*> fb

instance Monad Maybe where  
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  (Just x) >>= f = f x

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv a b = Just $ a / b

composeMaybe :: Double -> Maybe Double
composeMaybe d = do
  let x = 100
  y <- safeDiv x 5
  z <- safeDiv y d
  safeDiv z 8

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where  
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State fs) = State $ \s -> 
    let (a, newState) = fs s
    in (f a, newState)

instance Applicative (State s) where  
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)
  
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State sf) <*> (State sa) = State $ \s ->
    let (f, s') = sf s
        (a, s'') = sa s'
    in (f a, s'')

instance Monad (State s) where  
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State sa) >>= f = State $ \s ->
    let (a, newState) = sa s
    in runState (f a) newState

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State (const ((), s))

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

deposit :: Int -> State Int ()
deposit amount = modify (+ amount)

withdraw :: Int -> State Int Bool
withdraw amount = do
  balance <- get
  if balance < amount
  then return False
  else True <$ modify (subtract amount)

session :: State Int String
session = do
  deposit 100
  ok1 <- withdraw 30
  deposit 50
  ok2 <- withdraw 150
  deposit 80
  return $ "Withdraw 30: " ++ show ok1 ++ "Withdraw 150: " ++ show ok2