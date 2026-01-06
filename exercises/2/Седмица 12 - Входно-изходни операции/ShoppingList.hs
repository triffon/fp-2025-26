module Main where

data Command = 
  Add String Int |
  Remove String |
  List |
  Exit

type ShoppingList = [(String, Int)]

data ParsingError =
  InvalidCommand |
  InvalidInteger

parseCommand :: String -> Either ParsingError Command
parseCommand str = case words str of
  ["list"] -> Right List
  ["exit"] -> Right Exit
  ["remove", arg] -> Right $ Remove arg
-- ...
  _ -> Left InvalidCommand 

parseInt :: String -> Either ParsingError Int
parseInt str = case reads str of
  [(n, "")] -> Right n
  _ -> Left InvalidInteger

parseShoppingList :: String -> Either ParsingError ShoppingList
parseShoppingList str = traverse parseItem $ lines str
  where
    parseItem :: String -> Either ParsingError (String, Int)
    parseItem str = case words str of
      -- [name, ]
  

main :: IO ()
main = undefined