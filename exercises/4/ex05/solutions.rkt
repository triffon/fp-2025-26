#lang racket

; =============================================================================
; Упражнение 5: Асоциативни списъци. Дървета
; =============================================================================

; =============================================================================
; 1. АСОЦИАТИВНИ СПИСЪЦИ (ASSOCIATION LISTS)
; =============================================================================

; Задача 1.1: Добавяне на двойка (ключ . стойност) в началото
(define (add-entry key value alist)
  (cons (cons key value) alist))

; Задача 1.2: Премахване на двойка по ключ
(define (remove-entry key alist)
  (filter (lambda (pair) (not (equal? (car pair) key))) alist))

; Задача 1.3: Обновяване/добавяне по ключ
(define (update-entry key new-value alist)
  (add-entry key new-value (remove-entry key alist)))

; Примери:
; (define phone-book '((alice . "555-1234") (bob . "555-5678") (charlie . "555-9012")))
; (add-entry 'david "555-3456" phone-book)
; (remove-entry 'bob phone-book)
; (update-entry 'alice "555-9999" phone-book)

; =============================================================================
; 2. ДЪРВЕТА (TREES)
; =============================================================================

; Представяне: списък, чийто car е стойност, а cdr е списък от поддървета
(define (tree-value tree) (car tree))
(define (tree-children tree) (cdr tree))
(define (leaf? tree) (null? (tree-children tree)))

; Задача 2.1: Брой на възлите
(define (tree-size tree)
  (let ([children (tree-children tree)])
    (if (null? children)
        1
        (+ 1 (foldr (lambda (child acc) (+ (tree-size child) acc)) 0 children))))
)

; Задача 2.2: Височина на дърво (брой нива)
(define (tree-height tree)
  (let ([children (tree-children tree)])
    (if (null? children)
        1
        (+ 1 (apply max (map tree-height children))))))

; Задача 2.3: Списък от листата
(define (tree-leaves tree)
  (let ([children (tree-children tree)])
    (if (null? children)
        (list (tree-value tree))
        (apply append (map tree-leaves children))))
)

; Задача 2.4: Сума на стойностите (за дърво от числа)
(define (tree-sum tree)
  (let ([children (tree-children tree)])
    (if (null? children)
        (tree-value tree)
        (+ (tree-value tree)
           (foldr (lambda (child acc) (+ (tree-sum child) acc)) 0 children))))
)

; Задача 2.5: tree-map - прилага функция върху всяка стойност
(define (tree-map f tree)
  (cons (f (tree-value tree))
        (map (lambda (child) (tree-map f child)) (tree-children tree))))

; Задача 2.6: Намира първа стойност, удовлетворяваща предикат; иначе #f
(define (tree-find pred tree)
  (if (pred (tree-value tree))
      (tree-value tree)
      (ormap (lambda (child) (tree-find pred child))
             (tree-children tree))))

; =============================================================================
; 3. КОМБИНИРАНИ ЗАДАЧИ
; =============================================================================

; Задача 3.1: Проверка за балансирано дърво
; Балансирано: за всеки възел, макс разлика във височини на децата <= 1
(define (tree-balance tree)
  (define (balanced-and-height t)
    (let ([children (tree-children t)])
      (if (null? children)
          (cons #t 1)
          (let* ([results (map balanced-and-height children)]
                 [balanced-all (andmap car results)]
                 [heights (map cdr results)]
                 [hmax (apply max heights)]
                 [hmin (apply min heights)]
                 [balanced-h (<= (- hmax hmin) 1)])
            (cons (and balanced-all balanced-h)
                  (+ 1 hmax))))))
  (car (balanced-and-height tree)))

; Задача 3.2: Броене на срещанията – връща асоциативен списък (число . брой)
(define (count-occurances lst)
  (foldl (lambda (x acc)
           (let ([pair (assoc x acc)])
             (if pair
                 (update-entry (car pair) (+ 1 (cdr pair)) acc)
                 (add-entry x 1 acc))))
         '()
         lst))

; =============================================================================
; ТЕСТОВЕ (примерни извеждания)
; =============================================================================

(define simple-tree '(1 (2 (4) (5)) (3 (6) (7)) (8)))

(displayln "=== Асоциативни списъци ===")
(define phone-book '((alice . "555-1234") (bob . "555-5678") (charlie . "555-9012")))
(displayln (add-entry 'david "555-3456" phone-book))
(displayln (remove-entry 'bob phone-book))
(displayln (update-entry 'alice "555-9999" phone-book))

(displayln "\n=== Дървета ===")
(displayln (tree-size simple-tree))      ; 8
(displayln (tree-height simple-tree))    ; 3
(displayln (tree-leaves simple-tree))    ; '(4 5 6 7 8)
(displayln (tree-sum simple-tree))       ; 36
(displayln (tree-map (lambda (x) (* 2 x)) simple-tree))
(displayln (tree-find even? simple-tree))

(displayln "\n=== Комбинирани ===")
(displayln (tree-balance simple-tree)) ; #t
(displayln (tree-balance '(1 (2) (3))))  ; #t
(displayln (tree-balance '(1 (2 (4)) (3)))) ; #t
(displayln (tree-balance '(1 (2 (4 (5))) (3)))) ; #f
(displayln (count-occurances '(1 2 2 3 3 3 4)))


