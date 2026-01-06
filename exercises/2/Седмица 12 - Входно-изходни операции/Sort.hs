module Main where

parseList :: IO [Int]
parseList = do
  line <- getLine
  return $ map read $ words line

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = let less = filter (< x) xs
                  greater = filter (>= x) xs
  in sort less ++ [x] ++ sort greater

main :: IO ()
main = parseList >>= putStrLn . unwords . map show . sort