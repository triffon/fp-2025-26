#lang racket

;; Упражнение 3: Функции от по-висок ред. Анонимни функции

;; ============================================================
;; Задача 1: apply-n-times
;; ============================================================
;; Прилага функция f върху x общо n пъти

(define (apply-n-times f n x)
  (if (= n 0)
      x
      (apply-n-times f (- n 1) (f x))))

;; Тестове:
(define (add-3 x) (+ x 3))
(define (square x) (* x x))

; (apply-n-times add-3 3 5)   ; => 14
; (apply-n-times square 2 2)  ; => 16

;; Допълнение: Намери ((5 + 3) * 2) * 2 с lambda
; (apply-n-times (lambda (x) (* x 2)) 2 (+ 5 3))  ; => 32


;; ============================================================
;; Задача 2: compose
;; ============================================================
;; Връща композиция на две функции: (f ∘ g)(x) = f(g(x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Тестове:
(define square-plus-3 (compose add-3 square))
; (square-plus-3 5)  ; => 28

;; Допълнение: Създай функция за (x + 1)^2 и я приложи за 4
(define add-one-squared (compose square (lambda (x) (+ x 1))))
; (add-one-squared 4)  ; => 25


;; ============================================================
;; Задача 3: partial-apply
;; ============================================================
;; Частично прилагане на първия аргумент на двуаргументна функция

(define (partial-apply f x)
  (lambda (y)
    (f x y)))

;; Тестове:
(define add-5 (partial-apply + 5))
; (add-5 7)  ; => 12

(define multiply-by-2 (partial-apply * 2))
; (multiply-by-2 4)  ; => 8


;; ============================================================
;; Задача 4.1: accumulate-interval с term функция
;; ============================================================
;; Натрупва стойности в интервал [a, b], като прилага term функция
;; над всеки елемент преди да го комбинира с combiner

(define (accumulate-interval combiner null-value term a b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-interval combiner null-value term (+ a 1) b))))

;; Тестове:
; (accumulate-interval + 0 square 1 5)  ; => 55 (1+4+9+16+25)


;; ============================================================
;; Задача 4.2: product-interval
;; ============================================================
;; Произведение на всички числа в интервала [a, b]

(define (product-interval a b)
  (accumulate-interval * 1 (lambda (x) x) a b))

;; Тестове:
; (product-interval 1 5)   ; => 120 (факториел на 5)
; (product-interval 3 6)   ; => 360 (3*4*5*6)


;; ============================================================
;; Задача 4.3: sum-odd-squares
;; ============================================================
;; Сума на квадратите на нечетните числа в интервала [a, b]

(define (sum-odd-squares a b)
  (accumulate-interval +
                       0
                       (lambda (x)
                         (if (odd? x)
                             (square x)
                             0))
                       a
                       b))

;; Тестове:
; (sum-odd-squares 1 5)   ; => 35 (1+9+25)
; (sum-odd-squares 2 10)  ; => 164 (9+25+49+81)


;; ============================================================
;; Задача 4.4: count-divisors-in-interval
;; ============================================================
;; Брой на числата в интервала [a, b], които са делители на n

(define (count-divisors-in-interval n a b)
  (accumulate-interval +
                       0
                       (lambda (x)
                         (if (= (remainder n x) 0)
                             1
                             0))
                       a
                       b))

;; Тестове:
; (count-divisors-in-interval 12 1 12)  ; => 6 (1,2,3,4,6,12)
; (count-divisors-in-interval 20 1 10)  ; => 5 (1,2,4,5,10)


;; ============================================================
;; Примери за изпълнение
;; ============================================================

(displayln "=== Задача 1: apply-n-times ===")
(displayln (apply-n-times add-3 3 5))   ; => 14
(displayln (apply-n-times square 2 2))  ; => 16
(displayln (apply-n-times (lambda (x) (* x 2)) 2 (+ 5 3)))  ; => 32

(displayln "\n=== Задача 2: compose ===")
(displayln (square-plus-3 5))  ; => 28
(displayln (add-one-squared 4))  ; => 25

(displayln "\n=== Задача 3: partial-apply ===")
(displayln (add-5 7))  ; => 12
(displayln (multiply-by-2 4))  ; => 8

(displayln "\n=== Задача 4.1: accumulate-interval ===")
(displayln (accumulate-interval + 0 square 1 5))  ; => 55

(displayln "\n=== Задача 4.2: product-interval ===")
(displayln (product-interval 1 5))   ; => 120
(displayln (product-interval 3 6))   ; => 360

(displayln "\n=== Задача 4.3: sum-odd-squares ===")
(displayln (sum-odd-squares 1 5))   ; => 35
(displayln (sum-odd-squares 2 10))  ; => 164

(displayln "\n=== Задача 4.4: count-divisors-in-interval ===")
(displayln (count-divisors-in-interval 12 1 12))  ; => 6
(displayln (count-divisors-in-interval 20 1 10))  ; => 5

