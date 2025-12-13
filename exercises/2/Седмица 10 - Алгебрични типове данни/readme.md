# Седмица 10 - Алгебрични типове данни

## Задача 01 - Maybe
Напишете своя версия на вградения тип `Maybe` и за нея напишете следните функции:

- `fromMaybe :: a -> Maybe a -> a` - приема стойност по подразбиране и стойност от тип `Maybe`. Ако вторият аргумент е `Just`, то функцията връща стойността му, в противен случай връща стойността по подразбиране
- `maybe :: b -> (a -> b) -> Maybe a -> b` - приема стойност по подразбиране, едноместна функция `f` и стойност от тип `Maybe`. Ако третият аргумент е `Just`, то функцията да прилага `f` върху стойността, в противен случай да върне стойността по подразбиране
- `catMaybes :: [Maybe a] -> [a]` - приема списък от стойности от тип `Maybe` и връща списък от всички `Just` стойности
- `mapMaybe :: (a -> Maybe b) -> [a] -> [b]` - приема функция `f` на един аргумент, която връща стойност от тип `Maybe` и списък. Функцията да прилага `f` над всички елементи и като резултат да върне само получените `Just` стойности
- `find :: (a -> Bool) -> [a] -> Maybe a` - намира първото срещане на елемент в списък по подаден предикат
- `lookup :: Eq a => a -> [(a, b)] -> Maybe b` - търси стойност в асоциативен списък по подаден ключ. Ако ключът не бъде намерен, връща `Nothing`

### Примери:
```hs
ghci> fromMaybe 3 $ Just 5 -- 5
ghci> fromMaybe 3 Nothing -- 3

ghci> maybe 3 (^ 2) $ Just 5 -- 25
ghci> maybe 3 (^ 2) Nothing

ghci> catMaybes [Just 3, Nothing, Just 5, Just 6 ,Nothing] -- [3,5,6]
ghci> mapMaybe (\x -> if odd x then (Just x) else Nothing) [3,5,6,4] -- [3,5]

ghci> find ((== "hello") . snd) [(2, "bye"), (5, "hello"), (1, "see you")] -- Just (5,"hello")
ghci> find ((== "greetings") . snd) [(2, "bye"), (5, "hello"), (1, "see you")]-- Nothing

ghci> lookup 2 [(1, "first"), (2, "second"), (3, "third")] -- Just "second"
ghci> lookup 4 [(1, "first"), (2, "second"), (3, "third")] -- Nothing
```

## Задача 02 - Either
> Either is what's right or whatever's left

Напишете своя версия на вградения тип `Either` и за нея напишете следните функции:

- `fromRight :: b -> Either a b -> b` - приема стойност по подразбиране и стойност от тип `Either`. Ако вторият аргумент е `Right`, то функцията връща стойността му, в противен случай връща стойността по подразбиране
- `either :: (a -> c) -> (b -> c) -> Either a b -> c` - приема две едноместни функции `f` и `g` и стойност от тип `Either`. Ако третият аргумент е `Left`, то функцията да прилага `f` върху стойността иначе да прилага `g`
- `rights :: [Either a b] -> [b]` - приема списък от стойностти от тип `Either` и връща списък от всички `Right` стойности
- `partitionEithers :: [Either a b] -> ([a],[b])` - приема списък от стойностти от тип `Either` и връща наредена двойка от списъци, съдържащи съответно всички `Left` и всички `Right` стойности
- `safeIndex :: [a] -> Int -> Either String a` - приема списък и индекс в списъка и връща елемента на подадения индекс. Ако индексът е отрицателен да се върне съобщение `"Index cannot be negative."`. Ако индексът е по-голям от размера на списъка да се върне съобщение `"Index out of bounds."`.


### Примери:
```hs
ghci> fromRight 3 $ Right 5 -- 5
ghci> fromRight 3 $ Left "hello"

ghci> either (++ " world") (("number: " ++) . show) $ Right 5 -- "number: 5"
ghci> either (++ " world") (("number: " ++) . show) $ Left "hello" -- "hello world"

ghci> rights [Left "foo", Right 3, Left "bar", Right 7, Left "baz"] -- [3,7]
ghci> partitionEithers [Left "foo", Right 3, Left "bar", Right 7, Left "baz"] -- (["foo","bar","baz"],[3,7])
```

## Задача 03 - Естествено число
Напишете потребителски тип `Nat`, представляващ естествено число. Чрез него напишете следните функции:

- `plus` - събира 2 естествени числа
- `multiply` - умножава 2 естествени числа
- `toInt` - конвертира естествено число към цяло
- `fromInt` - конвертира цяло число към естествено, ако е възможно. Използвайте типа `Maybe`

### Примери:
```hs
ghci> let five = Succ $ Succ $ Succ $ Succ $ Succ Zero
ghci> let three = Succ $ Succ $ Succ Zero

ghci> toInt $ plus five three -- 8
ghci> toInt $ multiply five three -- 15
ghci> toInt $ multiply Zero three -- 0

ghci> fromInt 3 -- Just (Succ (Succ (Succ Zero)))
ghci> fromInt (-4) -- Nothing
```

## Задача 04 - Двоично дърво
Напишете тип `BinaryTree` представляващ двоично дърво. Чрез него напишете следните функции:

- `rotateLeft/rotateRight :: BinaryTree a -> BinaryTree a` - извършва лява/дясна ротация на двоично дърво
- `bloom :: BinaryTree a -> BinaryTree a` - заменя всяко листо на двоично дърво с дърво, съдържащо само 2 листа, като всички възли в него имат стойност, същата като тази на замененото листо
- `paths :: BinaryTree a -> [[a]]` - по подадено двоично дърво, връща списък от всички пътища от корена до някое листо на дървото
- `maxSumPath :: (Num a, Ord a) => BinaryTree a -> a` - по подадено двоично дърво с числа във възлите, намира максималната сума на числата по някой път от корен до листо. **Бонус:** реализирайте функцията като обходите всеки възел на дървото точно 1 път.

### Примери:
```hs
testTree :: BinaryTree Int
testTree = Node 5 
                (Node 1 
                      (Node 4 
                            Empty 
                            (Node 13 Empty Empty)) 
                      (Node 3 Empty Empty)) 
                (Node 8 
                      (Node 0 
                            (Node 10 Empty Empty) 
                            (Node 9 Empty Empty)) 
                      (Node 11 Empty Empty))

{-
        5
      /   \
     1    8
    / \  / \
   4  3 0  11
   \   / \
   13 10 9
-}

ghci> rotateLeft testTree -- Node 8 (Node 5 (Node 1 (Node 4 Empty (Node 13 Empty Empty)) (Node 3 Empty Empty)) (Node 0 (Node 10 Empty Empty) (Node 9 Empty Empty))) (Node 11 Empty Empty)
ghci> rotateRight testTree -- Node 1 (Node 4 Empty (Node 13 Empty Empty)) (Node 5 (Node 3 Empty Empty) (Node 8 (Node 0 (Node 10 Empty Empty) (Node 9 Empty Empty)) (Node 11 Empty Empty)))
ghci> bloom testTree -- Node 5 (Node 1 (Node 4 Empty (Node 13 (Node 13 Empty Empty) (Node 13 Empty Empty))) (Node 3 (Node 3 Empty Empty) (Node 3 Empty Empty))) (Node 8 (Node 0 (Node 10 (Node 10 Empty Empty) (Node 10 Empty Empty)) (Node 9 (Node 9 Empty Empty) (Node 9 Empty Empty))) (Node 11 (Node 11 Empty Empty) (Node 11 Empty Empty)))
ghci> paths testTree -- [[5,1,4,13],[5,1,3],[5,8,0,10],[5,8,0,9],[5,8,11]]
ghci> maxSumPath testTree -- 24
```

## Задача 05 - Двоично дърво за търсене
Напишете тип `BST` представляващ двоично дърво за търсене. Чрез него напишете следните функции:

- `search :: Ord t => t -> BST t -> Bool` - търси елемет в двоично наредено дърво
- `insert :: Ord t => t -> BST t -> BST t` - добавя елемент към двоично наредено дърво;
- `remove :: Ord t => t -> BST t -> BST t` - премахва елемент от двоично наредено дърво;
- `kthSmallest :: Ord t => BST t -> Int -> t` - намира k-тия по големина елемент в двоично наредено дърво (Бонус: напишете безопасен вариант с Maybe);
- `rangeSearch :: Ord t => t -> t -> BST t -> [t]` - връща списък от тези елементи от двоично наредено дърво, които са част от подадения интервал;
- `kClosestElements :: Ord a => a -> Int -> BST a -> [a]` - връща списък от първите k елементи, които са най-близки до даден елемент в двоично наредено дърво.

```hs
bst :: BST Integer
bst = BSTNode 3 (BSTNode 1 
                         (BSTNode 0 BSTEmpty BSTEmpty) 
                         (BSTNode 2 BSTEmpty BSTEmpty))
                (BSTNode 5 
                         BSTEmpty 
                         (BSTNode 6 BSTEmpty BSTEmpty))
```

### Упътване:
На практика `BST` ще има същата структура като `BinaryTree`, понеже няма как да наложим по-строги ограничения върху типа. На практика разликата между двата типа ще дойде от функциите, реализирани за тях.