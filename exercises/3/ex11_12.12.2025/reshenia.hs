
data Color
  = Red
  | Green
  | Blue
  deriving (Show)

data Point а = Pt а а а
  deriving (Show)

data List a
  = Nil
  | Cons a (List a)

instance Show a => Show (List a) where
  show Nil = []
  show (Cons x xs) = show x ++ " " ++ show xs

mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (Cons h t) = Cons (f h) (mapList f t)

instance Foldable List where
    foldr :: (a -> b -> b) -> b -> List a -> b
    foldr _ nv Nil = nv
    foldr f nv (Cons x xs) = f x (foldr f nv xs)

data BTree a
  = Leaf
  | Node a (BTree a) (BTree a)
    deriving (Eq)

instance Show a => Show (BTree a) where
    show Leaf = ""
    show (Node x left right) = "(" ++ show x ++ show left ++ show right ++ ")"

allPaths :: BTree a -> [[a]]
allPaths Leaf = []
allPaths (Node x Leaf Leaf) = [[x]]
allPaths (Node x left right) = map (x:) (allPaths left ++ allPaths right)

instance Foldable BTree where
    foldr _ nv Leaf = nv
    foldr f nv (Node x left right) = foldr f (f x (foldr f nv right)) left

instance Eq a => Eq (List a) where
 (==) :: Eq a => List a -> List a -> Bool
 Nil == Nil = True
 _ == Nil = False
 Nil == _ = False
 Cons x xs == Cons y ys = x == y && xs == ys

len :: List a -> Int
len = foldr (\x res -> res + 1) 0


testTree = Node 5
                (Node 1
                      (Node 4
                            Leaf
                            (Node 13 Leaf Leaf))
                      (Node 3 Leaf Leaf))
                (Node 8
                      (Node 0
                            (Node 10 Leaf Leaf)
                            (Node 9 Leaf Leaf))
                      (Node 11 Leaf Leaf))

data Nat -- от Natural number (естествено число)
  = Zero
  | Succ Nat
  deriving (Show)

instance Eq Nat where
    (==) :: Nat -> Nat -> Bool
    Zero == Zero = True 
    _ == Zero = False
    Zero == _ = False 
    Succ x == Succ y = x == y

instance Ord Nat where 
    (<=) :: Nat -> Nat -> Bool 
    Zero <= _ = True 
    _ <= Zero = False 
    Succ x <=  Succ y = x  <= y 

plus :: Nat -> Nat -> Nat
plus Zero x = x
plus x Zero = x 
plus (Succ x) y = plus x (Succ y)

mult :: Nat -> Nat -> Nat 
mult Zero _ = Zero 
mult _ Zero = Zero 
mult (Succ x) y = plus y (mult x y) 

natToInteger :: Nat -> Int
natToInteger Zero = 0
natToInteger (Succ z) = natToInteger z + 1

integerToNat :: Int -> Nat
integerToNat 0 = Zero
integerToNat z = Succ (integerToNat (z - 1))

integerToNat' :: Int -> Maybe Nat
integerToNat' 0 = Just Zero
integerToNat' z
    | z < 0 = Nothing
    | otherwise = Just (Succ (integerToNat (z - 1)))
