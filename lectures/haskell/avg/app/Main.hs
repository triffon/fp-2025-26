module Main where
import Utils (getInt, readAndSum, readInt)
import Control.Monad

findAverage :: IO Double
findAverage = do n <- readInt "брой"
                 l <- mapM (readInt.("число #"++).show) [1..n]
                 let s = sum l
                 return $ fromIntegral s / fromIntegral n

main = forever $
       do avg <- findAverage
          putStrLn $ "Средното аритметично е: " ++ show avg
          putStrLn "Хайде отново!"
