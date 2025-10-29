# Упражнение 5 – Списъци и функции от по-висок ред върху тях, дълбоки списъци, дървета

## `foldr` и `foldl` -  като `accumulate`, но за списъци

```
(define (foldr operation null_value lst)
  (if (null? lst) null_value
      (operation (car lst)
          (foldr operation null_value (cdr lst)))))

(define (foldl operation null_value lst)
  (if (null? lst) null_value
      (foldl operation (operation null_value (car lst)) (cdr lst))))
```

### Задача 0
Да се напишат функциите `reverse` , `map`  и `filter` чрез **`foldr`** и **`foldl`**.

### Задача 1
Да се напише функция `least`, която намира най-малкото число в даден списък от числа.
```
(least '(4 5 1 8 3 -9 -4 1 4)) ; -> -9
```

### Задача 2

Да се напише функция `unique l`, която връща уникалните елементи в даден списък. Няма значение реда на елементите.
```
(unique '(1 3 6 2 2 8 1)) ; -> ‘(3 6 2 8 1)
(unique '(0 "asd" 2 "asd" "1" 0 4)) ; ->  '(2 "asd" "1" 0 4)
```

### Задача 3

Да се напишaт функциите `union l1 l2` и `intersection l1 l2`, които намират съответно обединението и сечението на два списъка. Реда на елементите няма значение

```
(union '(1 2 3 4) '(5 2 3 1 7)) ; -> '(4 5 2 3 1 7)
(intersection '(1 2 3 4) '(5 2 3 1 7)) ; -> '(1 2 3)
```
### Задача 4

Да се напише функция `insert l x`, която вмъква x на правилното място в сортиран списък.
```
(insert 5 '(1 2 4 6 7)) ; -> '(1 2 4 5 6 7)
(insert 5 '(1 2 4)) ; -> '(1 2 4 5)
(insert 5 '(6 7)) ; '(5 6 7)

```

### Задача 5
Да се напише функция `insertion-sort l`, която сортира списък чрез съответният алгоритъм. Подсказка: може да стане и с `foldr`
```
(insertion-sort '(1 5 5 8 2 4 1 2 9 4 8)) ; -> '(1 1 2 2 4 4 5 5 8 8 9)
```

## Функцията `apply`	

```
(apply + ‘( 1 2 3 4)) ; еквиваленто на (+ 1 2 3 4)

```
Чрез `apply` можем да работим с произволен брой аргументи или да прилагаме функция приемаща произволен брой аргументи върху списъци.

### Задача 6
Да се напише функция `find-interval`, която приема произволен брой числа и намира разликата между най-голямото и най-малкото число.

```
(find-interval 2 5 8 -2) ; -> 10
(find-interval 4 6 2 4 1 3 9 12) ; -> 11
```
### Задача 7
Да се напише функция `min-lex`, която приема произволен брой стрингове и намира най-малкия лексикографски. 	
За сравняване на символи използвайте `string<?`.
```
(min-lex "asd" "asdd" "azcesd")
```

### Задача 8 

Да се напише функция `compose-all`, която приема произволен брой функции и връща тяхната композиция като резултат.
```
((compose-all2 ^2 succ succ) 5) ; -> 49
```

## Дълбоки списъци

```scheme
(define dl '((1 2) ((3 (4) 5) (((6)))) (7)))

(define (atom? l)
  (and (not (null? l)) (not (pair? l))))

(define (deep-foldr op term nv l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        (else (op (deep-foldr op term nv (car l))
                  (deep-foldr op term nv (cdr l))))))
```
### Задача 9
Да се напише функция `deep-map f dl`, която е като `map`, но за дълбоки списъци.
```
(deep-map (lambda (x) (* x x)) dl); -> '((1 4) ((9 (16) 25) (((36)))) (49))
```

### Задача 10
Да се напише функция `deep-filter p? dl`, която е като `filter`, но за дълбоки списъци.
```
(deep-filter even? dl); -> '((2) (((4)) (((6)))) ())
```

### Задача 11
Да се напише функция `deep-member? x dl `, която проверява дали даден елемент се съдържа в дълбок списък.
```
(deep-member? 7 dl); -> #t
(deep-member? 18 dl); -> #f
```
## Двоични дървета
Използваме следната имплементаци на двоични дървета:
```
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
```
Примерни дървета:
```

(define t (make-tree 5
                     (make-tree 1
                                (make-tree 4
                                           '()
                                           (make-leaf 13))
                                (make-leaf 3))
                     (make-tree 8
                                (make-tree 0
                                           (make-leaf 10)
                                           (make-leaf 9))
                                (make-leaf 11))))

```
### Задача 12

Да се напише функция `depth t`, която намира височината на дърво. Опитайте се да я решите с рекурсия, както и чрез `deep-foldr`.
```
(depth t); -> 4
```

### Задача 13

Да се напише функция `count-leafs t`, която намира броя на листата на двоично дърво.
```
(count-leafs t); -> 5
```

### Задача 14

Да се напише функция `traverse t`, която обхожда дървото ляво-корен-дясно и връща списък от елементите на дървото
```
(traverse t) ; -> '(4 13 1 3 5 10 0 9 8 11)
```
### Задача 15

Да се напише функция `level k t`, която връща елементите на дървото на ниво `k`. Нека броенето започва от 0-ла, тоест коренът на дървото се намира на ниво 0.
 
```
(level 1 t); -> (1 8)
(level 2 t) ; -> '(4 3 0 11)
(level 4 t2); -> ‘()
```




