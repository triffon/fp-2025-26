
data DeepList a = Atom a | List [DeepList a]
instance Show a => Show (DeepList a) where
  show (Atom x) = show x
  show (List xs) = show xs
deepMapCond :: DeepList a -> (a -> Int -> Bool) -> (a -> Int -> b) -> (a -> Int -> b) -> DeepList b
deepMapCond lst p f1 f2 = helper 0 lst
  where helper d (Atom x) = Atom $ (if p x d then f1 else f2) x d
        helper d (List xs) = List $ map (helper (d+1)) xs

dl, dl2 :: DeepList Int
dl = List [Atom 1, List [Atom 2, List [Atom 5, Atom 1], Atom 4], Atom 3]
dl2 = deepMapCond dl (>) (\x d -> d) (\x d -> x*2)

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq, Foldable)

mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (Cons h t) = Cons (f h) (mapList f t)

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap _ Nil = Nil
    fmap f (Cons h t) = Cons (f h) (mapList f t)

l1 = Cons 2 (Cons 4 (Cons 6 (Cons 7 Nil)))



data BTree a
  = Leaf
  | Node a (BTree a) (BTree a)
    deriving (Eq, Foldable)

instance Show a => Show (BTree a) where
    show Leaf = ""
    show (Node x left right) = "(" ++ show x ++ show left ++ show right ++ ")"

instance Functor BTree where
  fmap :: (a -> b) -> BTree a -> BTree b
  fmap _ Leaf = Leaf
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)


allPaths :: BTree a -> [[a]]
allPaths Leaf = []
allPaths (Node x Leaf Leaf) = [[x]]
allPaths (Node x left right) = map (x:) (allPaths left ++ allPaths right)


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

testBST = BSTNode 20
                (BSTNode 12
                      (BSTNode 6
                            BSTEmpty
                            (BSTNode 7 BSTEmpty BSTEmpty))
                      (BSTNode 17 BSTEmpty BSTEmpty))
                (BSTNode 35
                      (BSTNode 22
                            (BSTNode 21 BSTEmpty BSTEmpty)
                            (BSTNode 24 BSTEmpty BSTEmpty))
                      (BSTNode 40 BSTEmpty BSTEmpty))


data BST a = BSTEmpty
           | BSTNode a (BST a) (BST a)
           deriving (Eq, Show, Foldable, Functor)

bstInsert :: Ord a => a -> BST a -> BST a
bstInsert x BSTEmpty = BSTNode x BSTEmpty BSTEmpty
bstInsert x (BSTNode val left right) = if x < val then BSTNode val (bstInsert x left) right
else BSTNode val left (bstInsert x right)

bstSearch :: Ord a => a -> BST a -> Bool
bstSearch _ BSTEmpty = False
bstSearch x (BSTNode val left right)
  | x == val = True
  | x < val = bstSearch x left
  | otherwise = bstSearch x right

bstValues :: BST a -> [a]
bstValues BSTEmpty = []
bstValues (BSTNode val left right) = bstValues left ++ [val] ++ bstValues right

bstSize :: BST a -> Int
bstSize = length

fromList :: Ord a => [a] -> BST a
fromList = foldr bstInsert BSTEmpty

bstSort :: Ord a => [a] -> [a]
bstSort = bstValues . fromList


data Map k v = MEmpty | MNode k v (Map k v) (Map k v)

mapInsert :: Ord k => k -> v -> Map k v -> Map k v
mapInsert key val MEmpty = MNode key val MEmpty MEmpty
mapInsert key val (MNode k2 v2 left right) 
  | key == k2 = MNode k2 val left right
  | key < k2 = MNode k2 v2 (mapInsert key val left) right
  | otherwise = MNode k2 v2 left (mapInsert key val right)

mapSearch :: Ord k => k -> Map k v -> Maybe v
mapSearch _ MEmpty = Nothing
mapSearch key (MNode k2 v2 left right)
  | key == k2 = Just v2
  | key < k2 = mapSearch key left 
  | otherwise = mapSearch key right

instance Functor (Map k) where 
  fmap :: (a -> b) -> Map k a -> Map k b
  fmap f MEmpty = MEmpty
  fmap f (MNode key val left right) = MNode key (f val) (fmap f left) (fmap f right)

data MyMaybe a
  = MyNothing
  | MyJust a
  deriving (Show)

instance Functor MyMaybe where
  fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  fmap _ MyNothing = MyNothing
  fmap f (MyJust k) = MyJust (f k)


