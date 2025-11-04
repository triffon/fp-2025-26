#lang racket

; =============================================================================
; Упражнение 4: Наредени двойки. Списъци. Функции от по-висок ред над списъци
; =============================================================================

; =============================================================================
; 1. НАРЕДЕНИ ДВОЙКИ (PAIRS)
; =============================================================================

; Задача 1.1: Функция за разстояние между две точки
(define (distance p1 p2)
  (let ([x1 (car p1)]
        [y1 (cdr p1)]
        [x2 (car p2)]
        [y2 (cdr p2)])
    (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))

; Тест за задача 1.1
; (distance (cons 0 0) (cons 3 4)) ; => 5.0

; =============================================================================
; 2. СПИСЪЦИ (LISTS)
; =============================================================================

; Задача 2.1: Сума на квадратите на елементите в списък
(define (list-squares-sum lst)
  (if (null? lst)
      0
      (+ (expt (car lst) 2) (list-squares-sum (cdr lst)))))

; Алтернативно решение с foldr
(define (list-squares-sum-foldr lst)
  (foldr (lambda (x acc) (+ (expt x 2) acc)) 0 lst))

; Задача 2.2: Брой символи в низове от списък
(define (list-char-count lst)
  (if (null? lst)
      '()
      (cons (string-length (car lst)) (list-char-count (cdr lst)))))

; Алтернативно решение с map
(define (list-char-count-map lst)
  (map string-length lst))

; Тестове за задачи 2.1 и 2.2
; (list-squares-sum '(1 2 3 4)) ; => 30 (1+4+9+16)
; (list-char-count '("hello" "world" "scheme" "racket")) ; => (5 5 6 6)
; (list-char-count '("a" "bc" "def" "ghij")) ; => (1 2 3 4)

; =============================================================================
; 3. ФУНКЦИИ ОТ ПО-ВИСОК РЕД НАД СПИСЪЦИ
; =============================================================================

; Задача 3.1: Кубове на елементите в списък
(define (list-cubes lst)
  (map (lambda (x) (expt x 3)) lst))

; Задача 3.2: Кубове само на четните числа
(define (list-even-cubes lst)
  (map (lambda (x) (expt x 3))
       (filter even? lst)))

; Задача 3.3: Сума на кубовете на четните числа
(define (list-even-cubes-sum lst)
  (foldr + 0
         (map (lambda (x) (expt x 3))
              (filter even? lst))))

; Задача 3.4: Брой елементи, които удовлетворяват предикат
(define (count-predicate pred lst)
  (foldr (lambda (x acc)
           (if (pred x)
               (+ acc 1)
               acc))
         0 lst))

; Тестове за задачи 3.1-3.4
; (list-cubes '(1 2 3 4)) ; => (1 8 27 64)
; (list-even-cubes '(1 2 3 4)) ; => (8 64)
; (list-even-cubes-sum '(1 2 3 4)) ; => 72 (8+64)
; (count-predicate even? '(1 2 3 4 5 6)) ; => 3
; (count-predicate (lambda (x) (> x 3)) '(1 2 3 4 5)) ; => 2

; =============================================================================
; 4. КОМБИНИРАНИ ЗАДАЧИ
; =============================================================================

; Задача 5.1: Zip функция - сума на елементи на същите позиции
(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (+ (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

; Задача 5.2: Zip функция с map
(define (zip-map lst1 lst2)
  (map + lst1 lst2))

; Задача 5.3: Транспониране на матрица
(define (matrix-transpose matrix)
  (if (null? matrix)
      '()
      (apply map list matrix)))

; Задача 5.4: Изравняване на вложени списъци
(define (flatten lst)
  (cond
    [(null? lst) '()]
    [(list? (car lst))
     (append (flatten (car lst)) (flatten (cdr lst)))]
    [else
     (cons (car lst) (flatten (cdr lst)))]))

; Тестове за комбинирани задачи
; (zip '(1 2 3) '(4 5 6)) ; => (5 7 9)
; (zip-map '(1 2 3) '(4 5 6)) ; => (5 7 9)
; (matrix-transpose '((1 2 3) (4 5 6))) ; => ((1 4) (2 5) (3 6))
; (flatten '(1 (2 3) (4 (5 6)))) ; => (1 2 3 4 5 6)

; =============================================================================
; ДОПЪЛНИТЕЛНИ ПРИМЕРИ
; =============================================================================

(displayln "=== Тестове за наредени двойки ===")
(displayln (distance (cons 0 0) (cons 3 4)))

(displayln "\n=== Тестове за списъци ===")
(displayln (list-squares-sum '(1 2 3 4)))
(displayln (list-char-count '("hello" "world" "scheme" "racket")))
(displayln (list-char-count '("a" "bc" "def" "ghij")))

(displayln "\n=== Тестове за функции от по-висок ред ===")
(displayln (list-cubes '(1 2 3 4)))
(displayln (list-even-cubes '(1 2 3 4)))
(displayln (list-even-cubes-sum '(1 2 3 4)))
(displayln (count-predicate even? '(1 2 3 4 5 6)))
(displayln (count-predicate (lambda (x) (> x 3)) '(1 2 3 4 5)))

(displayln "\n=== Тестове за комбинирани задачи ===")
(displayln (zip '(1 2 3) '(4 5 6)))
(displayln (zip-map '(1 2 3) '(4 5 6)))
(displayln (matrix-transpose '((1 2 3) (4 5 6))))
(displayln (flatten '(1 (2 3) (4 (5 6)))))