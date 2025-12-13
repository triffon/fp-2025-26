


subsets2 :: [a] ->  [[a]]
subsets2 [] = [[]]
subsets2 (x:xs) = [] : map (x:) e ++ ne
 where e@(_:ne) = subsets2 xs


constFunc :: Rational -> Rational
constFunc x = x -- При делене на 0 ще хвърли грешка

constFunc2 :: Double -> Double
constFunc2 x = x -- При делене на 0 ще върне безкрайност - Infinity

fact :: (Eq t, Num t) => t -> t
fact 0 = 1
fact n = n * fact (n-1)

taylorElem :: Integer -> Double -> Double
taylorElem n x = x ^ n / fromIntegral (product [1..n])

expTaylor :: Double -> Double
expTaylor = foldr (\ f res y -> f y + res y) (const 0) (take 20 (map taylorElem [0..]))

---------------------------------------

x = (2 + 5, error "Грешка")

f x = f x

l = 1:2:f 0:[]

pairs = [ (x, y) | x <- [0..], y <- [0..x] ]

pairs2 = [ (x, y) | x <- [0..], y <- [0..], z <- [0..], x + y == z ]
pairs4 = [ (x, z) | x <- [0..], y <- [0..], z <- [0..], mod z 2 == y ]


pairs3 = [ (x, z - x) | z <- [0..], x <- [0..z] ]

----------------------------------------

pitagoreanTripples = [(x, y, z)| z <- [1..], y <- [1..z], x <- [1..y], x^2 + y^2 == z^2, gcd x y == 1]

filterTupels :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
filterTupels p xs ys = [ (xs !! fst pair, ys !! snd pair ) |pair <- pairs3, p (xs !! fst pair) ( ys !! snd pair) ]

repeat2 :: [a] -> [a]
repeat2 x = x ++ repeat2 x

main :: IO ()
main = do
    -- putStrLn "Enter numbers"
    -- numbersLine <- getLine

    -- let numbers = [read num :: Double| num <- words numbersLine]
    
    -- putStrLn ("There are " ++ show (length numbers) ++ " numbers.")

    putStrLn "Enter string: "
    str <- getLine
    putStrLn "Enter number of times to repeat: "
    timesLine <- getLine
    let times = read timesLine :: Int 




    putStrLn (take (times * length str) (repeat2 str))


streamZipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
streamZipWith f (x:xs) (y:ys) = f x y : streamZipWith f xs ys 

composeAlt f g = id : map (\ h -> f . h) (composeAlt f g) 


applyToAll :: [t -> a] -> t -> [a]
applyToAll fs x = map (\ f -> f x) fs