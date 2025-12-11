module Main where
main :: IO ()
-- main = putStrLn "Hello, world!"

main = do putStr "Hello,"
          putChar ' '
          putStrLn "world!"
