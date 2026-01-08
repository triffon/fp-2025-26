module Utils where

getInt :: IO Int
getInt = do line <- getLine
            return (read line)

readAndSum :: Int -> IO Int
readAndSum 0 = return 0
readAndSum n = do putStrLn "Моля, въведете число: "
                  x <- getInt
                  s <- readAndSum (n-1)
                  return (x + s)

readInt :: String -> IO Int
readInt s = do putStrLn $ "Моля, въведете " ++ s ++ ": "
               getInt

