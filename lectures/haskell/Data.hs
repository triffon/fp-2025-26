module Data where

-- >>> :t map
-- map :: (a -> b) -> [a] -> [b]
-- >>> :k Bool
-- Bool :: *
-- >>> 

-- >>> :k [[(Bool,Int)]]
-- [[(Bool,Int)]] :: *

type UnaryFunction a = a -> a

-- >>> :k UnaryFunction
-- UnaryFunction :: * -> *
-- >>> :k UnaryFunction Bool
-- UnaryFunction Bool :: *

type Dictionary k v = [(k,v)]
-- >>> :k Dictionary
-- Dictionary :: * -> * -> *
-- >>> :k Dictionary Char
-- Dictionary Char :: * -> *
-- >>> :k Dictionary Char String
-- Dictionary Char String :: *

-- >>> :t ()
-- () :: ()
-- >>> :k ()
-- () :: *
-- >>> :t []
-- [] :: [a]
-- >>> :k []
-- [] :: * -> *
-- >>> :k [] Int
-- [] Int :: *
-- >>> :k [Int]
-- [Int] :: *
-- >>> :k ([] ([] Int))
-- ([] ([] Int)) :: *
-- >>> :k 

-- >>> :t (,)
-- (,) :: a -> b -> (a, b)

-- >>> :k (,)
-- (,) :: * -> * -> *

-- >>> :k (,) Int Bool
-- (,) Int Bool :: *
-- (Int, Bool)

-- >>> :k (,) Int
-- (,) Int :: * -> *

-- >>> :k (->)
-- (->) :: * -> * -> *

-- >>> :k (->) Int
-- (->) Int :: * -> *

-- >>> :t 5
-- 5 :: Num a => a

-- >>> :t maxBound
-- maxBound :: Bounded a => a

-- >>> maxBound::Int
-- 9223372036854775807

-- >>> maxBound::Bool
-- True

-- >>> maxBound::Char
-- '\1114111'

-- >>> :t (+)
-- (+) :: Num a => a -> a -> a

-- >>> :t (/)
-- (/) :: Fractional a => a -> a -> a

-- >>> :t div
-- div :: Integral a => a -> a -> a

-- >>> :t (==)
-- (==) :: Eq a => a -> a -> Bool

-- >>> :t elem
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool

-- >>> :t show
-- show :: Show a => a -> String

-- >>> :k Num
-- Num :: * -> Constraint

class Measurable a where
    size :: a -> Integer
    empty :: a -> Bool
    -- empty x = size x == 0
    -- empty x = (0==) (size x)
    empty = (==0) . size

-- >>> :k Measurable
-- Measurable :: * -> Constraint

-- >>> :t size
-- size :: Measurable a => a -> Integer

-- >>> size [1..10]
-- No instance for `Measurable [Integer]' arising from a use of `size'
-- In the expression: size [1 .. 10]
-- In an equation for `it_aAOa': it_aAOa = size [1 .. 10]

-- >>> empty [1..10]
-- No instance for `Measurable [Integer]'
--   arising from a use of `empty'
-- In the expression: empty [1 .. 10]
-- In an equation for `it_aBxx': it_aBxx = empty [1 .. 10]

larger :: (Measurable a1, Measurable a2) => a1 -> a2 -> Bool
larger x y = size x > size y

instance Measurable Integer where
    size 0 = 0
    size n = 1 + size (div n 10)

-- >>> size 128347128937318237
-- 18

-- >>> empty 18127891478
-- False

instance (Measurable a, Measurable b) => Measurable (a, b) where
    size (x, y) = size x + size y

-- >>> size (8327128937,(812321,12312))
-- 21

instance Measurable a => Measurable [a] where
    -- size l = sum (map size l)
    size = sum . map size

-- >>> size [213,123,1551]
-- 10

-- "a", "aa", "aaa", "aaaa", ... 

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq, Ord, Enum, Read, Show)

today :: Weekday
today = Wed

-- >>> :t Tue
-- Tue :: Weekday

-- >>> :k Weekday
-- Weekday :: *

-- >>> today
-- Wed

-- >>> today == Thu
-- False

-- >>> Mon < Sun
-- True

-- >>> [Mon .. Fri]
-- [Mon,Tue,Wed,Thu,Fri]

x :: Weekday
x = read "Mon"

-- >>> x
-- Mon

data Unit = Unit

-- >>> :t Unit
-- Unit :: Unit

type Name = String
type Score = Int
-- data Player = Player Name Score
data Player = Player { name :: Name, score :: Score }
    deriving (Eq, Ord, Read, Show)

-- >>> :t Player
-- Player :: Name -> Score -> Player

-- >>> :k Player
-- Player :: *

katniss :: Player
-- katniss = Player "Katniss Everdeen" 45
katniss = Player { score = 45, name = "Katniss Everdeen" }

mario :: Player
mario = Player { score = 30, name = "Mario" }

-- >>> katniss < mario
-- True


-- >>> :t katniss
-- katniss :: Player

-- >>> katniss
-- Player {name = "Katniss Everdeen", score = 45}

-- >>> name katniss
-- "Katniss Everdeen"

-- >>> :t name
-- name :: Player -> Name

data Shape = Circle { radius :: Double } | Rectangle { width, height :: Double }
    deriving (Eq, Ord, Read, Show)

circle :: Shape
circle = Circle 2.3

-- >>> circle
-- Circle {radius = 2.3}

rectangle :: Shape
rectangle = Rectangle 5.8 3.5

-- >>> circle < rectangle
-- True

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

-- >>> area circle
-- 16.619025137490002

-- >>> area rectangle
-- 20.3

-- >>> :t Just 5
-- Just 5 :: Num a => Maybe a

-- >>> :t (!!)
-- (!!) :: HasCallStack => [a] -> Int -> a

--- >>> :k Maybe
-- Maybe :: * -> *

data MyPair a b = MyPair a b
  deriving (Eq, Ord, Read, Show)

-- >>> :t MyPair
-- MyPair :: a -> b -> MyPair a b

-- >>> :k MyPair
-- MyPair :: * -> * -> *

-- >>> :k Either
-- Either :: * -> * -> *

data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Read, Show)

one = Succ Zero
two = Succ $ Succ Zero
-- >>> one > Zero
-- True

-- >>> two > one
-- True

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n

-- >>> fromNat two
-- 2

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n
  | n > 0 = Succ $ toNat $ n - 1

-- >>> toNat 10
-- Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))

plusNat :: Nat -> Nat -> Nat
plusNat Zero n = n
plusNat (Succ m) n = Succ $ plusNat m n

-- >>> fromNat $ plusNat (toNat 5) (toNat 8)
-- 13

data Bin = One | BitZero Bin | BitOne Bin
  deriving (Eq, Ord, Read, Show)

six = BitZero $ BitOne $ One

fromBin :: Bin -> Integer
fromBin One = 1
fromBin (BitZero b) = 2 * fromBin b
fromBin (BitOne  b) = 2 * fromBin b + 1

-- >>> fromBin six
-- 6

-- data RealBin = Zero | Pos Bin = Maybe Bin

toBin :: Integer -> Bin
toBin 1 = One
toBin n
  | even n    = BitZero $ toBin $ n `div` 2
  | otherwise = BitOne  $ toBin $ n `div` 2

-- >>> toBin 120
-- BitZero (BitZero (BitZero (BitOne (BitOne (BitOne One)))))

succBin :: Bin -> Bin
succBin One = BitZero One
succBin (BitZero b) = BitOne b
succBin (BitOne  b) = BitZero (succBin b)

-- >>> fromBin $ succBin $ toBin 5 
-- 6

--- data ([] a) = [] | (:) { head : a, tail : [a] } deriving (Eq, Ord, Read, Show)

data List a = Nil | Cons { listHead :: a, listTail :: List a }
    deriving (Eq, Ord, Read, Show)

l = Cons 1 $ Cons 2 $ Cons 3 $ Nil

-- >>> l
-- Cons {listHead = 1, listTail = Cons {listHead = 2, listTail = Cons {listHead = 3, listTail = Nil}}}

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

-- >>> :k List
-- List :: * -> *

(+++) :: List a -> List a -> List a
Nil         +++ l = l
(Cons x xs) +++ l = Cons x (xs +++ l)

--- >>> fromList $ l +++ l
-- [1,2,3,1,2,3]

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

-- >>> mapBinTree (+1) t
-- Node {root = 2, left = Node {root = 4, left = Empty, right = Empty}, right = Node {root = 6, left = Empty, right = Empty}}

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

data Tree a =  Tree { rootTree :: a, subtrees :: TreeList a }
    deriving (Eq, Ord, Read, Show)

data TreeList a = None | SubTree { firstTree :: Tree a, restTrees :: TreeList a }
    deriving (Eq, Ord, Read, Show)

leaf x = Tree x None
tree = Tree 1 $ SubTree (leaf 2)
              $ SubTree (Tree 3 $ SubTree (leaf 4) $ None)
              $ SubTree (leaf 5) $ None

-- >>> tree
-- Tree {rootTree = 1, subtrees = SubTree {firstTree = Tree {rootTree = 2, subtrees = None}, restTrees = SubTree {firstTree = Tree {rootTree = 3, subtrees = SubTree {firstTree = Tree {rootTree = 4, subtrees = None}, restTrees = None}}, restTrees = SubTree {firstTree = Tree {rootTree = 5, subtrees = None}, restTrees = None}}}}

level :: Integer -> Tree a -> [a]
level 0 (Tree x _) = [x]
level n (Tree _ ts) = levelTrees (n - 1) ts

levelTrees :: Integer -> TreeList a -> [a]
levelTrees _ None         = []
levelTrees n (SubTree t ts) = level n t ++ levelTrees n ts

-- >>> level 0 tree
-- [1]

-- >>> map (`level` tree) [0..4]
-- [[1],[2,3,5],[4],[],[]]

data SExpr = SBool Bool | SChar Char | SInt Int |
             SDouble Double | SList { list :: [SExpr] }
             deriving (Eq, Ord, Show, Read)

sexpr = SList [SInt 2, SChar 'a', SList [SBool True, SDouble 1.2, SList []]]

countAtoms :: SExpr -> Integer
countAtoms (SList ses) = sum $ map countAtoms ses
countAtoms _           = 1

-- >>> countAtoms sexpr
-- 4

sconcat :: [SExpr] -> SExpr
sconcat ses = SList $ concatMap list ses

flatten :: SExpr -> SExpr
-- collect :: SExpr -> [SExpr]
flatten (SList ses) = sconcat $ map flatten ses
flatten atom        = SList [atom]

-- >>> flatten sexpr
-- SList {list = [SInt 2,SChar 'a',SBool True,SDouble 1.2]}
