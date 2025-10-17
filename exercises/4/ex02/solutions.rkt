#lang racket

; =============================================================================
; Упражнение 2: Опашкова рекурсия. Модел на средите. Вложени дефиниции. let, let*
; =============================================================================

; =============================================================================
; 1. ОПАШКОВА РЕКУРСИЯ (Tail Recursion)
; =============================================================================

; Задача 4.1: sum-down-from с опашкова рекурсия
(define (sum-down-from-helper n acc)
    (if (= n 0)
        acc
        (sum-down-from-helper (- n 1) (+ n acc))))

(define (sum-down-from n)
  (sum-down-from-helper n 0))

; Задача 4.2: string-repeat с опашкова рекурсия
(define (string-repeat-helper str n acc)
    (if (= n 0)
        acc
        (string-repeat-helper str (- n 1) (string-append acc str))))

(define (string-repeat str n)
  (string-repeat-helper str n ""))

; Задача 4.3: fib с опашкова рекурсия

(define (fib-iter n i fi fi-1)
  (if (= i n)
      fi
      (fib-iter n (+ i 1) (+ fi fi-1) fi)))

(define (fib n)
  (if (= n 0)
      0
      (fib-iter n 1 1 0)))

; =============================================================================
; 2. МОДЕЛ НА СРЕДИТЕ (Environment Model)
; =============================================================================

; Задача 2.1: closure-demo
(define a "global-a")
(define b "global-b")
(define c "global-c")
(define scope (string-append a "," b "," c))

(define (closure-demo)
  (define a "closure-a")
  (define scope (string-append a "," b "," c))

  (define (closure-demo-2)
    (define b "closure-2-b")
    (define scope (string-append a "," b "," c))

    (define (closure-demo-3)
      (define a "closure-3-a")
      (define scope (string-append a "," b "," c))
      scope
    )

    (string-append scope " -> " (closure-demo-3)))

  (string-append scope " -> " (closure-demo-2))
)

; Очакван резултат: "closure-a,global-b,global-c -> closure-a,closure-2-b,global-c -> closure-3-a,closure-2-b,global-c"

; =============================================================================
; 3. ВЛОЖЕНИ ДЕФИНИЦИИ (Nested Definitions)
; =============================================================================

; Задача 3.1: Рефакториране на опашковата рекурсия с вложени функции

; sum-down-from с вложена функция
(define (sum-down-from-nested n)
  (define (sum-helper n acc)
    (if (= n 0)
        acc
        (sum-helper (- n 1) (+ n acc))))
  (sum-helper n 0))

; string-repeat с вложена функция
(define (string-repeat-nested str n)
  (define (repeat-helper str n acc)
    (if (= n 0)
        acc
        (repeat-helper str (- n 1) (string-append acc str))))
  (repeat-helper str n ""))

; fib с вложена функция
(define (fib-nested n)
  (define (fib-iter i fi fi-1)
    (if (= i n)
        fi
        (fib-iter (+ i 1) (+ fi fi-1) fi)))
  (if (= n 0)
      0
      (fib-iter 1 1 0)))

; Задача 3.2: cylinder-volume с вложени дефиниции
(define (cylinder-volume radius height)
  (define pi 3.14159)
  (define (circle-area r)
    (* pi r r))
  (define base-area (circle-area radius))
  (* base-area height))

; =============================================================================
; 4. LET КОНСТРУКЦИИ
; =============================================================================

; Задача 4.1: sum-of-powers с let
(define (sum-of-powers a b power)
  (let
      ((a-power (expt a power))
       (b-power (expt b power)))
       (+ a-power b-power)))

; Задача 4.2: cylinder-volume с let*
(define (cylinder-volume-let* radius height)
  (let* ((pi 3.14159)
         (base-area (* pi radius radius)))
    (* base-area height)))

; Задача 4.3: closure-demo с let*
(define (closure-demo-let*)
  (let* ((a "closure-a")
         (scope (string-append a "," b "," c)))
    (let* ((b "closure-2-b")
           (scope (string-append a "," b "," c)))
      (let* ((a "closure-3-a")
             (scope (string-append a "," b "," c)))
        scope))))

; =============================================================================
; 5. КОМБИНИРАНИ БОНУС ЗАДАЧИ
; =============================================================================

; Задача 5.1: efficient-power с O(log n) сложност
(define (efficient-power x n)
  (define (power-helper x n acc)
    (cond
      [(= n 0) acc]
      [(even? n) (power-helper (* x x) (/ n 2) acc)]
      [else (power-helper x (- n 1) (* acc x))]))
  (power-helper x n 1))

; Задача 5.2: palindrome? - проверка за палиндром в дадена бройна система
(define (palindrome? base n)
  (define (number-to-base num base)
    (if (= num 0)
        '()
        (cons (remainder num base) (number-to-base (quotient num base) base))))

  (define (reverse-list lst)
    (define (reverse-helper lst acc)
      (if (null? lst)
          acc
          (reverse-helper (cdr lst) (cons (car lst) acc))))
    (reverse-helper lst '()))

  (define (equal-lists? lst1 lst2)
    (cond
      [(and (null? lst1) (null? lst2)) #t]
      [(or (null? lst1) (null? lst2)) #f]
      [(= (car lst1) (car lst2)) (equal-lists? (cdr lst1) (cdr lst2))]
      [else #f]))

  (let ((digits (number-to-base n base)))
    (equal-lists? digits (reverse-list digits))))

; =============================================================================
; ТЕСТОВЕ И ПРИМЕРИ
; =============================================================================

; Тестове за опашкова рекурсия
(displayln "=== Тестове за опашкова рекурсия ===")
(displayln (format "sum-down-from 5: ~a" (sum-down-from 5))) ; 15
(displayln (format "string-repeat \"Hello\" 3: ~a" (string-repeat "Hello" 3))) ; "HelloHelloHello"
(displayln (format "fib 7: ~a" (fib 7))) ; 13
(displayln (format "fib 10: ~a" (fib 10))) ; 55

; Тест за closure-demo
(displayln "\n=== Тест за closure-demo ===")
(displayln (format "closure-demo: ~a" (closure-demo)))
(displayln (format "closure-demo-let*: ~a" (closure-demo-let*)))

; Тестове за let конструкции
(displayln "\n=== Тестове за let конструкции ===")
(displayln (format "sum-of-powers 2 3 2: ~a" (sum-of-powers 2 3 2))) ; 4 + 9 = 13
(displayln (format "cylinder-volume-let* 2 5: ~a" (cylinder-volume-let* 2 5))) ; ~62.83

; Тестове за бонус задачи
(displayln "\n=== Тестове за бонус задачи ===")
(displayln (format "efficient-power 2 10: ~a" (efficient-power 2 10))) ; 1024
(displayln (format "gcd-comparison 48 18: ~a" (gcd-comparison 48 18))) ; (6 6)
(displayln (format "factorial-comparison 5: ~a" (factorial-comparison 5))) ; (120 120)
(displayln (format "palindrome? 10 121: ~a" (palindrome? 10 121))) ; #t
(displayln (format "palindrome? 2 5: ~a" (palindrome? 2 5))) ; #t (101 в двоична)
(displayln (format "palindrome? 10 123: ~a" (palindrome? 10 123))) ; #f
