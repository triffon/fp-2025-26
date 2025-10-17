#lang racket

(define (5+ x) (+ x 5))

(define (3* x) (* x 3))
(define (id x) x)

(define (fake-compose f g x)
  (f (g x)))

(define (compose f g) (lambda (x) (f (g x))))

(define (flip f) (lambda (x y) (f y x)))

(define (repeat f n)
  (if (= n 0) id
      (compose f (repeat f (- n 1)))))


(define (twist n f g)
  (repeat (compose f g) (/ n 2)))

(define (twist2 n f g)
  (if (= n 0) id
      (compose f (twist2 (- n 1) g f)))) 

(define (++ x) (+ x 1))
(define (sq x) (* x x))
(define foo (twist 4 ++ sq))
(define bar (twist 2 ++ sq))
(define foo2 (twist2 4 ++ sq))
(define bar2 (twist2 2 ++ sq))

(define (curry f)
  (lambda (x) (lambda (y) (f x y))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (fact-acc n) (accumulate * 1 1 n id ++))

(define (!! n)
  (accumulate * 1 (if (odd? n) 1 2) n id (lambda (x) (+ 2 x))))

(define (sum-odd-squares a b)
  (accumulate + 0 (if (odd? a) a (++ a)) b sq (lambda (x) (+ 2 x))))

(define (binomial-fact n k)
  (/ (fact-acc n) (* (fact-acc k) (fact-acc (- n k)))))

(define (binomial n k)
  (accumulate * 1 1 k (lambda (x) (/ (- (+ 1 n) x) x)) ++))

(define (test-max f x y)
  (if (< (f x) (f y)) y x))

(define (argmax f a b)
  (accumulate (lambda (x y) (if (< (f x) (f y)) y x)) a a b id ++))

(define (all? p? a b)
  (accumulate (lambda (x y) (and x y)) #t a b p? ++))

(define (any p? a b)
  (accumulate (lambda (x y) (or x y)) #f a b p? ++))

(define (prime? n)
  (if (< n 2) #f
  (all? (lambda (x) (not (zero? (remainder n x)))) 2 (/ n 2))))

(define (repeat2 f n)
  (accumulate compose id 1 n (lambda (x) f) ++))

(define (twist3 n f g)
  (accumulate compose id 1 n (lambda (x) (if (odd? x) g f)) ++))

(define (count-digits n)
  (if (zero? n) 1
  (accumulate + 0 1 n (lambda (x) 1) (lambda (x) (* x 10)))))

(define (test_error x y)
  (if (= y 0) (error "Division by 0")
      (/ x y)))

(define custom-sum (lambda x (apply + x)))

