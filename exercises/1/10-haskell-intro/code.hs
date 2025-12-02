{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

foo :: Int
foo = 42

bar :: Integer
bar = 42 ^ 300

baz :: Bool
baz = foo == 43

c1 :: Char
c1 = 'щ'

add :: Int -> Int -> Int
add a b = a + b

isEven :: Int -> Bool
isEven x = (mod x 2) == 0

isSumEven :: Int -> (Int -> Bool)
isSumEven a b = isEven (a + b)

myid :: foo -> foo
myid x = x

mymin :: (Ord a) => a -> a -> a
mymin x y
    | x < y     = x
    | otherwise = y

mylist :: [Int]
mylist = [1, 2, 3, 4]

qux :: (Int, Bool, Char)
qux = (42, True, 'a')

baba :: [Char]
baba = ['b', 'a', 'b', 'a']

dyado :: [Char]
dyado = "dyado"


fact :: Int -> Int
fact 0 = 1
fact x = x * (fact (x - 1))

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

myAbs :: Int -> Int
myAbs x
    | x < 0     = -x
    | otherwise = x

myAbs2 :: Int -> Int
myAbs2 x = if x < 0 then -x else x

composeInt :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
composeInt f g = result
    where
        result :: Int -> Int
        result x = f (g x)

composeInt2 :: (Int -> Int) -> (Int -> Int) -> Int -> Int
composeInt2 f g x = f (g x)

compose :: (c -> b) -> (a -> c) -> a -> b
compose f g x = f (g x)

myConcat' :: [a] -> [a] -> [a]
myConcat' [] l = l
myConcat' l1 l2 = (head l1) : (myConcat (tail l1) l2)

myConcat :: [a] -> [a] -> [a]
myConcat [] l = l
myConcat (x:xs) l = x : (myConcat xs l)

isIntPrefix :: [Int] -> [Int] -> Bool
isIntPrefix [] _ = True
isIntPrefix _ [] = False
isIntPrefix (x:xs) (y:ys) = (x == y) && (isIntPrefix xs ys)

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = (x == y) && (isPrefix xs ys)

frepeat :: Int -> (a -> a) -> a -> a
frepeat 0 _f x = x
frepeat n f x = f (frepeat (n - 1) f x)

frepeated :: Int -> (a -> a) -> a -> a
frepeated n f x = f (frepeat (n - 1) f x)

frepeated' :: Int -> (a -> a) -> a -> a
frepeated' = frepeat

frepeated'' :: Int -> (a -> a) -> a -> a
frepeated'' n f = frepeat n (compose f) id

frepeat' :: Int -> (a -> a) -> a -> a
frepeat' = frepeated''
