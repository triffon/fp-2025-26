module Main where

-- 1. Собствени списъци
data List a
  = Empty
  | Cons a (List a)
  deriving (Show, Eq)

listMap :: (a -> b) -> List a -> List b
listMap _ Empty = Empty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)

-- 2. Класът Functor
instance Functor List where
  fmap _ Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 3. Двоични дървета
data Tree a
  = EmptyTree
  | Node a (Tree a) (Tree a)
  deriving (Show, Read, Eq)

-- Задача 3.1
isEmpty :: Tree a -> Bool
isEmpty EmptyTree = True
isEmpty _ = False

-- Задача 3.2
size :: Tree a -> Int
size EmptyTree = 0
size (Node _ l r) = 1 + size l + size r

-- Задача 3.3
-- Помощна функция за конкатенация на нашите List
appendList :: List a -> List a -> List a
appendList Empty ys = ys
appendList (Cons x xs) ys = Cons x (appendList xs ys)

leafsList :: Tree a -> List a
leafsList EmptyTree = Empty
leafsList (Node x EmptyTree EmptyTree) = Cons x Empty
leafsList (Node _ l r) = appendList (leafsList l) (leafsList r)

-- Задача 3.4
instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- 4. BST
-- Задача 4.1
bstInsert :: (Ord a) => a -> Tree a -> Tree a
bstInsert val EmptyTree = Node val EmptyTree EmptyTree
bstInsert val (Node x l r)
  | val < x = Node x (bstInsert val l) r
  | otherwise = Node x l (bstInsert val r)

-- Задача 4.2
bstSearch :: (Ord a) => a -> Tree a -> Bool
bstSearch _ EmptyTree = False
bstSearch val (Node x l r)
  | val == x = True
  | val < x = bstSearch val l
  | otherwise = bstSearch val r

-- Задача 4.3
-- Помощна функция за конкатенация на стандартни списъци е (++)
bstValues :: Tree a -> [a]
bstValues EmptyTree = []
bstValues (Node x l r) = bstValues l ++ [x] ++ bstValues r

-- Задача 4.4
isSymmetric :: (Eq a) => Tree a -> Bool
isSymmetric EmptyTree = True
isSymmetric (Node _ l r) = isMirror l r
  where
    isMirror EmptyTree EmptyTree = True
    isMirror (Node _ l1 r1) (Node _ l2 r2) = isMirror l1 r2 && isMirror r1 l2
    isMirror _ _ = False

-- 5. Expr
data Expr
  = Val Int
  | Add Expr Expr
  | Mult Expr Expr
  | IfZero Expr Expr Expr
  deriving (Show)

eval :: Expr -> Int
eval (Val n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mult e1 e2) = eval e1 * eval e2
eval (IfZero cond thenExpr elseExpr) =
  if eval cond == 0
    then eval thenExpr
    else eval elseExpr

-- Тестови данни
t :: Tree Int
t = Node 5 (Node 3 EmptyTree EmptyTree) (Node 7 EmptyTree EmptyTree)

symTree :: Tree Int
symTree =
  Node
    3
    (Node 2 (Node 1 EmptyTree EmptyTree) EmptyTree)
    (Node 4 EmptyTree (Node 5 EmptyTree EmptyTree))

-- Note: Това дърво от README-то не е точно симетрично по стойности (1 и 5),
-- но е симетрично по структура ако гледаме само формата.
-- Функцията isMirror горе игнорира стойностите (Node _ ...), така че проверява само структурата.
-- Ако искаме да е симетрично и по стойности, трябва 1 и 5 да са равни.
-- Ще ползвам пример, който е структурно симетричен за теста.

main :: IO ()
main = do
  putStrLn "--- 1. List Map ---"
  let l = Cons 1 (Cons 2 Empty)
  print $ listMap (+ 1) l

  putStrLn "\n--- 2. Functor List ---"
  print $ fmap (* 10) l
  print $ 5 <$ l

  putStrLn "\n--- 3. Tree Basics ---"
  print $ "Tree: " ++ show t
  print $ "isEmpty t: " ++ show (isEmpty t)
  print $ "isEmpty EmptyTree: " ++ show (isEmpty EmptyTree)
  print $ "size t: " ++ show (size t)
  print $ "leafsList t: " ++ show (leafsList t)

  putStrLn "\n--- 3. Functor Tree ---"
  print $ fmap (* 2) t

  putStrLn "\n--- 4. BST ---"
  let t2 = bstInsert 4 t
  print $ "Inserted 4: " ++ show t2
  print $ "Search 3 in t: " ++ show (bstSearch 3 t)
  print $ "Search 9 in t: " ++ show (bstSearch 9 t)
  print $ "Values of (bstInsert 4 t): " ++ show (bstValues t2)

  putStrLn "\n--- 4.4 Symmetric ---"

  print $ "Is symmetric: " ++ show (isSymmetric symTree)

  putStrLn "\n--- 5. Expr ---"
  let expr = Mult (Add (Val 3) (Val 5)) (Val 2)
  print $ "Expr: " ++ show expr
  print $ "Eval: " ++ show (eval expr)

  -- if (5 + (-5)) == 0 then 10 else 20  => 10
  let ifExpr = IfZero (Add (Val 5) (Val (-5))) (Val 10) (Val 20)
  print $ "IfZero Expr: " ++ show ifExpr
  print $ "Eval IfZero: " ++ show (eval ifExpr)
