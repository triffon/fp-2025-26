module Main where

main :: IO ()
main = getLine >>= (putStrLn . unwords . reverse . words)

-- main = do
--   line <- getLine
--   let reversed = reverse $ words line
--   putStrLn $ unwords reversed

