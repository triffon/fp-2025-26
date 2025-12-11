module Main where
import Utils (getInt, readAndSum)

findAverage :: IO Double
findAverage = do n <- getInt
                 s <- readAndSum n
                 return (fromIntegral s / fromIntegral n)

main :: IO ()
main = do putStrLn "Моля, въведете брой числа"
          a <- findAverage
          putStrLn "Средното аритметично е: "
          print a
