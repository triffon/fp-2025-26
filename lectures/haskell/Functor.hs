module Functor where

import Prelude hiding (Functor, fmap, (<$>),
                       Applicative, pure, (<*>), liftA2, sequenceA,
                       Alternative, empty, (<|>))

-- >>> :k Functor
-- Functor :: (* -> *) -> Constraint

class Functor f where
   fmap :: (a -> b) -> f a -> f b
   fmap = (<$>)

   (<$>) :: (a -> b) -> f a -> f b
   (<$>) = fmap

instance Functor Maybe where
   -- fmap :: (a -> b) -> Maybe a -> Maybe b
   fmap _ Nothing  = Nothing
   fmap f (Just x) = Just $ f x

-- >>> (+1) $ 3 
-- 4

-- >>> (+1) $ Just 3
-- No instance for `Num (Maybe Integer)'
--   arising from a use of `it_aGKN'
-- In the first argument of `evalPrint', namely `it_aGKN'
-- In a stmt of an interactive GHCi command: evalPrint it_aGKN

-- >>> fmap (+1) (Just 3)
-- Just 4

-- >>> (+1) <$> Just 3
-- Just 4

-- >>> :k (,)
-- (,) :: * -> * -> *

-- >>> :k (,) Int
-- (,) Int :: * -> *

instance Functor ((,) a) where
    -- >>> fmap :: (b -> c) -> (a, b) -> (a, c)
    f <$> (x, y) = (x, f y)

-- >>> (+1) <$> (2, 5)
-- (2,6)

newtype ReversedPair a b = ReversedPair (b, a)
  deriving Show

instance Functor (ReversedPair b) where
    -- >>> fmap :: (a -> c) -> ReversedPair b a -> ReversedPair b c
    fmap f (ReversedPair (x, y)) = ReversedPair (f x, y)

-- >>> (+1) <$> ReversedPair (2, 5)
-- ReversedPair (3,5)

instance Functor (Either a) where
--  fmap :: (b -> c) -> Either a b -> Either a c
  _ <$> Left x  = Left x
  f <$> Right y = Right $ f y

-- >>> (+1) <$> Left "err"
-- Left "err"

-- >>> (+1) <$> Right 3
-- Right 4

instance Functor [] where
    fmap = map

-- >>> (+1) <$> [1..5]
-- [2,3,4,5,6]

data BinTree a = Empty | Node { root :: a, left, right :: BinTree a }
    deriving (Eq, Ord, Read, Show)

leafBin x = Node x Empty Empty
t = Node 1 (leafBin 3) (leafBin 5)

depth :: BinTree a -> Integer
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

-- >>> depth t
-- 2

leaves :: BinTree a -> [a]
leaves Empty = []
leaves (Node x Empty Empty) = [x]
leaves (Node _ l r) = leaves l ++ leaves r

-- >>> leaves t
-- [3,5]

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree _ Empty = Empty
mapBinTree f (Node x l r) = Node (f x) (mapBinTree f l) (mapBinTree f r)

instance Functor BinTree where
    fmap = mapBinTree

-- >>> (+1) <$> t
-- Node {root = 2, left = Node {root = 4, left = Empty, right = Empty}, right = Node {root = 6, left = Empty, right = Empty}}

instance Functor ((->) r) where
--    fmap :: (a -> b) -> (r -> a) -> r -> b
    fmap = (.)

-- >>> ((+1) <$> (*2)) 5
-- 11

instance Functor IO where
--    fmap :: (a -> b) -> IO a -> IO b
   fmap f io = do x <- io
                  return $ f x


class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  -- fmap = (<*>) . pure

instance Applicative Maybe where
  -- pure  :: a -> Maybe a
  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    pure = Just
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just f <*> Just x = Just $ f x

-- >>> (+) <$> Just 2 <*> Just 3
-- Just 5

instance Applicative (Either a) where
--   pure  :: b -> Either a b
--  (<*>) :: Either a (b -> c) -> Either a b -> Either b c
   pure = Right
   Left x <*> _ = Left x
   _ <*> Left x = Left x
   Right f <*> Right x = Right $ f x


-- >>> (+) <$> Right 2 <*> Right 3
-- Right 5

-- >>> (+) <$> Left "error" <*> Right 3
-- Left "error"

-- >>> (+) <$> Left "error1" <*> Left "error2"
-- Left "error1"

instance Applicative [] where
  -- pure  :: a -> [a]
  -- (<*>) :: [a -> b] -> [a] -> [b]
  pure x = [x]
  -- fs <*> xs = [ f x | f <- fs, x <- xs ]
  fs <*> xs = concatMap (<$> xs) fs

-- >>> (+) <$> [1,2,3] <*> [10, 20, 30]
-- [11,21,31,12,22,32,13,23,33]

instance Applicative ((->) r) where
  -- pure  :: a -> r -> a
  -- (<*>) :: (r -> a -> b) -> (r -> a) -> r -> b
  pure = const
  (f <*> g) x = f x (g x)

-- >>> ((+) <$> (*2) <*> (^3)) 3
-- 33

instance Applicative IO where
  pure = return
  -- (<*>) :: IO (a -> b) -> IO a -> IO b
  fio <*> xio = do f <- fio
                   x <- xio
                   return $ f x

liftA2 :: Applicative f => (a1 -> a2 -> b) -> f a1 -> f a2 -> f b
liftA2 f x y = f <$> x <*> y

-- >>> liftA2 (+) (Just 2) (Just 3)
-- Just 5

-- >>> (+) 2 3
-- 5

-- >>> sequenceA [Just 2, Just 3, Just 5]
-- Just [2,3,5]

-- >>> sequenceA [Just 2, Nothing, Just 5]
-- Nothing


-- >>> sequenceA [] :: Maybe [a]
-- Just []

sequenceA :: Applicative f => [f a] -> f [a]
{-
sequenceA []     = pure []
sequenceA (x:xs) = liftA2 (:) x $ sequenceA xs
-}
sequenceA = foldr (liftA2 (:)) $ pure []

-- liftA2 (:) Just 2 Just [3, 5] ---> Just [2,3,5]
-- 2 : [3,5]   --> [2,3,5]

-- >>> sequenceA [[5,6]]
-- [[5],[6]]

-- >>> sequenceA [[3,4],[5,6]]
-- [[3,5],[3,6],[4,5],[4,6]]

-- >>> sequenceA [[1,2],[3,4],[5,6]]
-- [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]

class Applicative f => Alternative f where
   empty :: f a
   (<|>) :: f a -> f a -> f a

instance Alternative Maybe where
   empty = Nothing
   Just x  <|> _ = Just x
   Nothing <|> y = y

instance Alternative [] where
  empty = []
  (<|>) = (++)