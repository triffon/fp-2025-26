module Main where
main :: IO ()
-- main = putStrLn "Hello, world!"

transformPlus :: Int -> Int -> IO Int 
transformPlus a b = return (a + b)

{-
main = do name <- getLine
          putStr "Hello, "
          putStr name
--          five <- transformPlus 2 3
          let five = 2 + 3
          putStr (show five)
          putStrLn "!"
          getLine
          return ()
-}

main = do putStrLn "Моля, въведете палиндром: "
          line <- getLine
          let revLine = reverse line
          if revLine == line then putStrLn "Благодаря!"
          else do putStrLn (line ++ " не е палиндром!")
                  main

getInt :: IO Int
getInt = do line <- getLine
            return (read line)