module Applications where

import Prelude hiding ((*>), lookup)
import Control.Applicative hiding ((*>))
import Control.Monad
import Data.Maybe

search :: (a -> Bool) -> [a] -> Maybe a 
search _ [] = Nothing
search p (x:xs) = mfilter p (Just x) <|> search p xs

-- >>> search odd [1..5]
-- Just 1

-- >>> search odd $ (*2) <$> [1..5]
-- Nothing

-- (and x1 x2 x3 ...) --> връща #f, ако има някое или последното ако всички са различти от #f    

{-
(*>) :: Maybe a1 -> Maybe a2 -> Maybe a2
Nothing *> _ = Nothing
_       *> x = x
-}

(*>) :: Applicative f => f a -> f b -> f b
y *> x = (const id <$> y) <*> x

-- >>> Just 2 *> Just 3 *> Just 5
-- Just 5

-- >>> Just 2 *> Nothing *> Just 5
-- Nothing

type AL k v = [(k,v)]

lookup :: Eq k => k -> AL k v -> Maybe v
lookup key = fmap snd . search ((key ==) . fst)

al = map (\k -> (k, 10*k)) [1..5]

-- >>> al
-- [(1,10),(2,20),(3,30),(4,40),(5,50)]

-- >>> lookup 3 al
-- Just 30

-- >>> lookup 6 al
-- Nothing

-- >>> lookup 1 al *> lookup 5 al
-- Just 50

type Tree a = AL a [a]

t = [(1,[2,3]),(2,[4,5]), (3,[6,7])]

children :: Eq a => a -> Tree a -> Maybe [a]
children = lookup

--- >>> children 1 t
-- Just [2,3]

parent :: Eq a => a -> Tree a -> Maybe a
parent x = fmap fst . search (elem x . snd)

-- >>> parent 2 t
-- Just 1

-- >>> parent 4 t
-- Just 2

-- >>> parent 1 t
-- Nothing

type BinTree a = AL a (a,a)

bt = [(1,(2,3)),(2,(4,7)),(3,(6,7))]

findPath :: Eq a => a -> a -> BinTree a -> Maybe [a]
findPath x y t
  | x == y = pure [x]
--  | otherwise = lookup x t *> ((x :) <$> (findPath l y t <|> findPath r y t))
--   where Just (l, r) = lookup x t 
  | otherwise = do (l, r) <- maybe empty pure $ lookup x t
                   (x :) <$> (findPath l y t <|> findPath r y t)

-- >>> findPath 1 7 bt
-- Just [1,2,7]

-- >>> :t maybeToList
-- maybeToList :: Maybe a -> [a]

-- >>> maybeToList Nothing
-- []

-- >>> maybeToList $ Just 2
-- [2]


findAllPaths :: Eq a => a -> a -> BinTree a -> [[a]]
findAllPaths x y t
  | x == y = pure [x]
--  | otherwise = maybeToList (lookup x t) *> ((x :) <$> (findAllPaths l y t <|> findAllPaths r y t))
--   where Just (l, r) = lookup x t 
  | otherwise = do (l, r) <- maybe empty pure $ lookup x t
                   (x :) <$> (findAllPaths l y t <|> findAllPaths r y t)

-- >>> findAllPaths 1 7 bt
-- [[1,2,7],[1,3,7]]

searchPath :: (MonadPlus m, Eq a) => a -> a -> BinTree a -> m [a]
searchPath x y t
  | x == y    = pure [x]
  | otherwise = do (l, r) <- maybe empty pure $ lookup x t
                   (x :) <$> (searchPath l y t <|> searchPath r y t)

-- >>> searchPath 1 7 bt
-- [1,2,7]

-- >>> searchPath 1 7 bt :: Maybe [Integer]
-- Just [1,2,7]

-- >>> searchPath 1 7 bt :: [[Integer]]
-- [[1,2,7],[1,3,7]

-- >>> searchPath 2 3 bt
-- *** Exception: user error (mzero)

-- >>> searchPath 2 3 bt :: Maybe [Integer]
-- Nothing

-- >>> searchPath 2 3 bt :: [[Integer]]
-- []

grandparent :: Eq a => a -> Tree a -> Maybe a
grandparent x t = do p <- parent x t
                     parent p t

-- >>> grandparent 7 t
-- Just 1

-- >>> grandparent 2 t
-- Nothing

grandchildren :: Eq a => a -> Tree a -> [a]
grandchildren x t = do cs <- maybeToList $ children x t
                       c <- cs
                       concat $ maybeToList $ children c t

-- >>> grandchildren 1 t