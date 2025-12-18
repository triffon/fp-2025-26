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

### Задача 1
Да се напише функция  `integerToNat’ :: Int -> Maybe Nat`, която превръща `Int` в `Nat`, като ако подаденото число е отрицателно да връща `Nothing`.
```haskell
ghci> integerToNat' 4
Just (Succ (Succ (Succ (Succ Zero))))
ghci> integerToNat' (-2)
Nothing
```

### Задача 2
Да се напише функция  `minus' :: Nat -> Nat -> Maybe Nat`, която намира разлика на две естествени числа, само ако тя съществува, иначе връща `Nothing`.
```haskell
ghci> minus (Succ Zero) (Succ (Succ Zero))
Nothing
ghci> minus (Succ (Succ Zero)) (Succ Zero)
Just (Succ Zero)
```

### Задача 3
Да се напише функция `find :: (t -> Bool) -> [t] -> Maybe t`, която намира първото срещане на елемент, който изпълнява даден предикат
```haskell
ghci> find even [1, 2, 3]
Just 2
ghci> find even [1, 3]
Nothing
```

### Задача 4
Да се напише функция `lookup :: Eq k => k -> [(k, v)] -> Maybe v`, която търси стойност в асоциативен списък.
```haskell
ghci> lookup' 1 [(1, "a"), (2, "b"), (1, "c")]
Just "a"
ghci> lookup' 3 [(1, "a"), (2, "b"), (1, "c")]
Nothing
```
