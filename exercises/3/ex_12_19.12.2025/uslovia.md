# Упражнение 12
## Типът `Maybe`
```haskell
data Maybe a
  = Nothing
  | Just a
  deriving (Show)
```
`Maybе` може да си го представяте като конструктивно доказателство, т.е. ако нещо съществува не връщаме просто *Истина*, връщаме и свидетеля. Използва се за операции, които може да хвърлят изключения.
Например може да дефинираме безопасно деление на 0:
```haskell
safeDiv :: Int -> Int -> Maybe (Int, Int)
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `quot` y, x `rem` y)
```
```haskell
ghci> safeDiv 5 0
Nothing
ghci> safeDiv 21 5
Just (4,1)
ghci> maybe (0, 0) id (safeDiv 21 5) -- така може да работим с `Maybe` и да се оправяме с грешките  
(4,1)
ghci> maybe (0, 0) id (safeDiv 5 0)
(0,0)

```

### Задача 4
Да се напише функция  `integerToNat’ :: Int -> Maybe Nat`, която превръща `Int` в `Nat`, като ако подаденото число е отрицателно да връща `Nothing`.

### Задача 5
Да се напише функция `lookup :: Eq k => k -> [(k, v)] -> Maybe v`, която търси стойност в асоциативен списък.
