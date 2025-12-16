{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fold" #-}
import Prelude hiding (Maybe(..), Semigroup(..), Monoid(..))

data Temperature = Celsius Double | Fahrenheit Double

data Maybe a = Nothing | Just a deriving Show

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

instance Show Temperature where
  show :: Temperature -> String
  show (Celsius t) = show t ++ "°C"
  show (Fahrenheit t) = show t ++ "°F"

toCelsius :: Temperature -> Double
toCelsius (Celsius t) = t
toCelsius (Fahrenheit t) = (t - 32) * 5 / 9

(===) :: Double -> Double -> Bool
d1 === d2 = let epsilon = 1e-6
  in abs (d1 - d2) < epsilon

instance Eq Temperature where
  (==) :: Temperature -> Temperature -> Bool
  t1 == t2 = toCelsius t1 === toCelsius t2

instance Ord Temperature where
  compare :: Temperature -> Temperature -> Ordering
  t1 `compare` t2
    | t1 == t2 = EQ
    | otherwise = let c1 = toCelsius t1
                      c2 = toCelsius t2
    in if c1 < c2 then LT else GT

class Collectable c where
  collect :: (a -> Bool) -> c a -> [a]

  collectAll :: c a -> [a]
  collectAll = collect (const True)

  find :: (a -> Bool) -> c a -> Maybe a
  find p = listToMaybe . collect p

  forAll :: (a -> Bool) -> c a -> Bool
  forAll p = null . collect (not . p)

instance Collectable [] where  
  collect :: (a -> Bool) -> [a] -> [a]
  collect = filter

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving Show

instance Collectable BinaryTree where  
  collect :: (a -> Bool) -> BinaryTree a -> [a]
  collect _ Empty = []
  collect p (Node root left right) = 
    let rest = collect p left ++ collect p right
    in if p root then root:rest else rest

testTree :: BinaryTree Int
testTree = Node 5 
                (Node 1 
                      (Node 4 
                            Empty 
                            (Node 13 Empty Empty)) 
                      (Node 3 Empty Empty)) 
                (Node 8 
                      (Node 0 
                            (Node 10 Empty Empty) 
                            (Node 9 Empty Empty)) 
                      (Node 11 Empty Empty))

instance Foldable Maybe where  
  foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr _ nv Nothing = nv
  foldr op nv (Just x) = op x nv

instance Foldable BinaryTree where  
  foldr :: (a -> b -> b) -> b -> BinaryTree a -> b
  foldr _ nv Empty = nv
  foldr op nv (Node root left right) = foldr op (op root (foldr op nv right)) left

class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a
  
  mconcat :: [a] -> a
  mconcat = foldr (<>) mempty

-- newtype Sum = Sum Int

instance Semigroup Int where
  (<>) :: Int -> Int -> Int
  (<>) = (+)

instance Monoid Int where  
  mempty :: Int
  mempty = 0

data Nat = Zero | Succ Nat deriving Show

instance Semigroup Nat where
  (<>) :: Nat -> Nat -> Nat
  Zero <> n = n
  (Succ a) <> b = Succ $ a <> b

instance Monoid Nat where
  mempty :: Nat
  mempty = Zero

five :: Nat
five = Succ $ Succ $ Succ $ Succ $ Succ Zero

three :: Nat
three = Succ $ Succ $ Succ Zero

instance Semigroup a => Semigroup (Maybe a) where 
  (<>) :: Semigroup a => Maybe a -> Maybe a -> Maybe a
  Nothing <> _ = Nothing
  _ <> Nothing = Nothing
  (Just a) <> (Just b) = Just $ a <> b

instance Monoid a => Monoid (Maybe a) where
  mempty :: Monoid a => Maybe a
  mempty = Just mempty

type Student = (String, Double, Int)

f :: Student -> Int
f (name, grade, year) = year