import Prelude hiding (Maybe(..), maybe, lookup, Either(..), either)

data Shape =
  Square Double Double |
  Triangle Double Double Double |
  Circle (Double, Double) Double

data Color = Red | Blue | Green

data Maybe a = Nothing | Just a deriving Show

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just x) = x

maybe :: b -> (a -> b) -> Maybe a -> b
maybe d _ Nothing = d
maybe _ f (Just x) = f x

catMaybes :: [Maybe a] -> [a]
catMaybes = map fromJust . filter isJust
  where
    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust _ = False

    fromJust :: Maybe a -> a
    fromJust (Just x) = x

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes . map f

find :: (a -> Bool) -> [a] -> Maybe a
find pred = listToMaybe . filter pred
  where
    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

data Either a b = Left a | Right b deriving Show

fromRight :: b -> Either a b -> b
fromRight d (Left _) = d
fromRight _ (Right x) = x

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left e) = f e
either _ f (Right v) = f v

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v

rights :: [Either a b] -> [b]
rights = mapMaybe eitherToMaybe

data IndexError = NegativeIndexError | IndexOutOfBoundsError deriving Show

safeIndex :: [a] -> Int -> Either IndexError a
safeIndex l index
  | index < 0 = Left NegativeIndexError
  | index > length l = Left IndexOutOfBoundsError
  | otherwise = Right $ l !! index

-- data List a = Empty | Cons a (List a)

data Nat = Zero | Succ Nat deriving Show

five :: Nat
five = Succ $ Succ $ Succ $ Succ $ Succ Zero

three :: Nat
three = Succ $ Succ $ Succ Zero

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ a) b = Succ $ plus a b

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ a) = 1 + toInt a

multiply :: Nat -> Nat -> Nat
multiply Zero _ = Zero
multiply _ Zero = Zero
multiply (Succ Zero) n = n
multiply (Succ a) b = plus b $ multiply a b

-- data BinaryTree a = Empty | Node { root :: a, left :: BinaryTree a, right :: BinaryTree a } deriving Show

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving Show

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

rotateLeft :: BinaryTree a -> BinaryTree a
rotateLeft Empty = Empty
rotateLeft t@(Node _ _ Empty) = t
rotateLeft (Node a t1 (Node b t2 t3)) = Node b (Node a t1 t2) t3

bloom :: BinaryTree a -> BinaryTree a
bloom Empty = Empty
bloom leaf@(Node root Empty Empty) = Node root leaf leaf
bloom (Node root left right) = Node root (bloom left) (bloom right)

paths :: BinaryTree a -> [[a]]
paths Empty = []
paths (Node root Empty Empty) = [[root]]
paths (Node root left right) = map (root:) $ paths left ++ paths right

data BST a = BSTEmpty | BSTNode a (BST a) (BST a) deriving Show

bst :: BST Integer
bst = BSTNode 3 (BSTNode 1 
                         (BSTNode 0 BSTEmpty BSTEmpty) 
                         (BSTNode 2 BSTEmpty BSTEmpty))
                (BSTNode 5 
                         BSTEmpty 
                         (BSTNode 6 BSTEmpty BSTEmpty))

search :: Ord t => t -> BST t -> Bool
search _ BSTEmpty = False
search x (BSTNode root left right)
  | x < root = search x left
  | x > root = search x right
  | otherwise = True

insert :: Ord t => t -> BST t -> BST t
insert x BSTEmpty = BSTNode x BSTEmpty BSTEmpty
insert x t@(BSTNode root left right)
  | x < root = BSTNode root (insert x left) right
  | x > root = BSTNode root left $ insert x right
  | otherwise = t

isEmpty :: BST a -> Bool
isEmpty BSTEmpty = True
isEmpty _ = False

remove :: Ord t => t -> BST t -> BST t
remove _ BSTEmpty = BSTEmpty
remove x (BSTNode root left right)
  | x < root = BSTNode root (remove x left) right
  | x > root = BSTNode root left (remove x right)
  | isEmpty left = right
  | isEmpty right = left
  | otherwise = let newRoot = rightmost left
    in BSTNode newRoot (remove newRoot left) right
  where
    rightmost :: BST t -> t
    rightmost (BSTNode x _ BSTEmpty) = x
    rightmost (BSTNode _ _ right) = rightmost right