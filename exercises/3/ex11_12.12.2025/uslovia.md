# Упражниение 11 - Алгебрични типове данни

Наричат се алгебрични заради начина си на дефиниране:
- “Сума” 
```haskell
data Color
  = Red
  | Green
  | Blue
  deriving (Show)  -- Този ред казва на Haskell да генерира нужните имплементации за Show.
``` 
Типовете от класа `Show` може да се принтират. Всеки ред представлява различен конструктор за типа. Съответно това са конструкторите, които можем да ползваме в за патерн мачинг. 


- “Произведение” - като наредена n-торка
```haskell
data Point а = Pt а а а
  deriving (Show)
```
`Point` наричаме типов конструктор, а `Pt` конструктор на данни. (Вместо `Pt` може да използваме `Point`, не е проблем, но трябва да се помни, че значат различни неща). 
Конструктора на данни използваме, когато създаваме стойности, а конструктора на типове, когато създаваме типове. `Point` не е тип сам по себе си -  нуждае е се от конкретен тип за `a`
```haskell
ghci> x = Pt 1 2 3
ghci> :t x -- показва типа на x, докато сме в интерпретатора
x :: Num а => Point а
ghci> y = Pt 1.2 2 3
ghci> :t y
y :: Fractional а => Point а
ghci> z = Pt (1::Int) 2 3
ghci> :t z
z :: Point Int
```
Типовете на `x` и `y` не са конкретни, защото Haskell не знае конкретния тип на числата, затова го ограничава до класове от типове. За `z` при дефиницията сме определили конкретния тип. 

Можем да си дефинираме наш си клас списък:

```haskell
data List a
  = Nil
  | Cons a (List a)
  deriving (Show)
```
В случая Haskell сам ще си изведе как да принтира на екрана стойности от тип `List`. 
Би излгеждало по следния начин:
```haskell
ghci> xs = Cons 1 (Cons 2 (Cons 4 Nil))
ghci> xs
Cons 1 (Cons 2 (Cons 4 Nil))
```
Но можем и сами да си дефинираме:

```haskell
instance Show a => Show (List a) where
 show Nil = []
 show (Cons x xs) = show x ++ " " ++ show xs
```
Съответно представянето ще е следното:
```haskell
ghci> xs = Cons 1 (Cons 2 (Cons 4 Nil))
ghci> xs
1 2 4 
```
По двата предни начина ние казваме на Haskell, че `List` е инстанция на класа `Show`, ако `a` също е от класа Show. За да кажем, че някой тип е от даден клас, то трябва да дефинираме за типа методите на класа. Чрез `:info <class>`  в интерпретатора може да видите нужните методи за класа. 

```haskell
ghci> :info Eq
type Eq :: * -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
        -- Defined in ‘GHC.Classes’
-- и т.н. има инстанции на класа, но само горното ни интересува
```
Може да го дефинираме за `List`:

```haskell
instance Eq a => Eq (List a) where
(==) :: Eq a => List a -> List a -> Bool
Nil == Nil = True
_ == Nil = False
Nil == _ = False
Cons x xs == Cons y ys = x == y && xs == ys
```
Вече можем да сравняваме списъци:
```haskell
ghci> xs = Cons 1 (Cons 2 (Cons 4 Nil))
ghci> ys = Cons 1 (Cons 2 (Cons 4 Nil))
ghci> zs = Cons 1 (Cons 2 (Cons 5 Nil))
ghci> xs == ys
True
ghci> xs == zs
False
```
### Задача 1
Да се напише тип `Nat`, който да е инстанция на `Ord`, `Show`  и `Eq`. 
Да се имплементират функциите `plus`, `mult`, `natToInteger` и `integerToNat ` за типа. `integerToNat` да работи за неотрицателни цели числа. 

### Задача 2.1
Да се напише функция `mapList`, която е като `map`, но за нашата дефиниция за `List`. 

```haskell 
ghci> xs = Cons 1 (Cons 2 (Cons 4 Nil))
ghci> mapList succ xs
2 3 5 
```
### Задача 2.2
Да се дефинира `List` като инстанция на `Foldable`, тоест да можем да използваме `foldr` върху нашия тип.

```haskell
ghci> xs = Cons 1 (Cons 2 (Cons 4 Nil))
ghci> foldr (\x res -> res + 1) 0 xs 
3
```

### Задача 3
Да се напише тип двоично дърво  `Btree`, като стойностите в дървото може да са от всякакъв тип. `Btree` да е инстанция на `Foldable`, `Show` и `Eq`. 

```haskell
ghci> t = Node 1 (Node 2 (Node 3 (Node 4 Leaf Leaf) Leaf) Leaf) (Node 5 (Node 6 (Node 7 (Node 8 Leaf Leaf) (Node 9 Leaf Leaf)) (Node 10 Leaf Leaf)) Leaf) 
ghci> t
(1(2(3(4)))(5(6(7(8)(9))(10)))) -- не е задължително да е такова представянето, както си решите сами
ghci> foldr (\x res -> res + 1) 0 t 
10
```

#### Задача 3.1
Да се напише функция, която връща всички пътища в дървото от корена до листата.
```haskell
ghci> allPaths t
[[1,2,3,4],[1,5,6,7,8],[1,5,6,7,9],[1,5,6,10]]
```

#### Задача 3.2 
Да се напише функция, която проверява дали елемент е част от дадено дърво. 

#### Задача 3.3 
Да се напише функция, която сумира елементите, ако са числа, в дадено дърво.

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
